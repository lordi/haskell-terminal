{-# LANGUAGE Rank2Types #-}
module Hsterm.Main where
import System.Process
import Data.Array.Unboxed
import Data.IORef
import Data.Char
import Data.Maybe (fromJust)
-- import Control.Monad
import Control.Monad.State hiding (state, get, State)
import System.IO

import Data.Time.Clock
import Data.Time.Calendar

import Graphics.UI.GLUT hiding (Bool, Float)
import Graphics.Rendering.OpenGL hiding (Bool, Float, get)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)
import Control.Concurrent
import Control.Applicative hiding (many)

import Terminal.Parser (parseANSI)
import Terminal.Terminal
import Terminal.Types
import qualified Terminal.Types as T

import Hsterm.State
import Hsterm.ShaderUtils

-- Constants (for now)
font = Fixed9By15
numColumns = 80
numRows = 24
screenWidth = 9 * numColumns
screenHeight = 16 * numRows

ansiColorToColor3 :: TerminalColor -> Color3 GLfloat
ansiColorToColor3 = fromJust . flip lookup x
        where x = [ (T.Black, Color3 0 0 0) 
                  , (T.Red, Color3 1 0 0)
                  , (T.Green, Color3 0 1 0)
                  , (T.Yellow, Color3 0 1 1)
                  , (T.Blue, Color3 0 0 1)
                  , (T.Magenta, Color3 1 1 0)
                  , (T.Cyan, Color3 1 0 1)
                  , (T.White, Color3 1 1 1) ] :: [(TerminalColor, Color3 GLfloat)]


initDisplay = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  createWindow "Haskell terminal emulator"
  windowSize $= Size screenWidth screenHeight
  clearColor $= Color4 0 0 0 1

unitQuad = do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 (-0) (-0)   0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (-0)   1    0 )
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1    1    0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1  (-0)   0 )

reshapeHandler size = do
  viewport $= (Position 0 0, Size screenWidth screenHeight)
  matrixMode $= Projection
  loadIdentity
  ortho (0::GLdouble) 1 1 0 0 1
  scale (1.0 / (fromIntegral numColumns)) ((1.0 / (fromIntegral numRows)) ::GLfloat) 1

-- |Select shader, prepare uniforms, blend full screen quad
blendQuadWithProgram prg (cy, cx)= do
  currentProgram $= Just prg
  let setUniform var val = do
      location <- get (uniformLocation prg var)
      reportErrors
      uniform location $= val
  setUniform "cursory" (Index1 ((fromIntegral cy) :: GLfloat))
  setUniform "cursorx" (Index1 ((fromIntegral cx) :: GLfloat))

  let time = getCurrentTime >>= return . realToFrac . utctDayTime
  timeInSeconds <- time
  setUniform "time" (Index1 (timeInSeconds :: GLfloat))

  preservingMatrix $ do
    color (Color3 0.04 0.04 0.10 :: Color3 GLfloat)
    scale (fromIntegral numColumns) (fromIntegral numRows) (1 ::GLfloat)
    unitQuad
  currentProgram $= Nothing

displayHandler :: State -> IO ()
displayHandler state = do
  term <- get $ terminal state

  clear [ColorBuffer]
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  matrixMode $= Modelview 0
  loadIdentity

  Just backgroundPrg <- get $ backgroundProgram state 
  blendQuadWithProgram backgroundPrg (cursorPos term)

  let withTextMode sth = do
      matrixMode $= Projection
      preservingMatrix $ do
        loadIdentity
        textureBinding Texture2D $= Nothing
        matrixMode $= Modelview 0
        preservingMatrix $ do
          loadIdentity
          sth

  -- Render a quad in the background color
  forM_ (indices $ screen term) $ \idx@(y, x) -> do
    preservingMatrix $ do
        translate (Vector3 (fromIntegral x - 1) (fromIntegral y - 1) (0 :: GLfloat))
        color $ ansiColorToColor3 (toEnum ((background term) ! idx))
        unitQuad

  -- Show the terminal buffer, this will be replaced by a more sophisticated
  -- text output that allows for zooming etc.
  chrWidth <- stringWidth font " "
  chrHeight <- fontHeight font
  color (Color3 0.8 0.8 0.8 :: Color3 GLfloat)
  withTextMode $ do
    let lines = chunk (cols term) $ elems (screen term)
        (relChrWidth, relChrHeight) = ((fromIntegral chrWidth) * 2.0 / (fromIntegral screenWidth), chrHeight * 2.0 / (fromIntegral screenHeight))
        rasterPosition (x, y) = Vertex4 (-1 + ((fromIntegral x - 1) * relChrWidth)) (1 - (fromIntegral y) * relChrHeight + 0.018) 0 (1::GLfloat)
    forM_ (zip [1..] lines) $ \(i, s) -> do
        currentRasterPosition $= rasterPosition (1, i)
        renderString font s

  Just cursorPrg <- get $ cursorProgram state 
  blendQuadWithProgram cursorPrg (cursorPos term)

  swapBuffers

-- | Standard build function.
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

chunk :: Int -> [s] -> [[s]]
chunk i ls = map (take i) (build (splitter ls)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

-- |Debug function to print the current terminal state to the console
printTerm term = do
    print $ (cursorPos term)
    putStrLn $ "," ++ (replicate (cols term) '_') ++ ","
    mapM_
        (putStrLn . (wrap "|"))
        (chunk (cols term) $ elems (screen term // [(cursorPos term, '|')]))
    putStrLn $ "`" ++ (replicate (cols term) '"') ++ "´"
    hFlush stdout
    where wrap d s = d ++ s ++ d

runTerminal :: IORef Terminal -> Handle -> Handle -> IO ()
runTerminal a in_ out = do
    forever $ do
        c <- (liftIO $ hGetChar out)
        s <- readIORef a

        -- Parse the input buffer for characters or ANSI sequences
        Right (actions, leftover) <- return $ parseANSI $ (inBuffer s) ++ [c]

        -- Apply all the actions to the terminal state
        forM actions $ \x -> modifyIORef a $ applyAction x

        -- Store the actions that could not be parsed as input buffer
        modifyIORef a $ \term -> term { inBuffer = leftover }
        
{-      s <- get
        when ((inBuffer s) /= "") $ do
            liftIO $ putStrLn "writing sth"
            liftIO $ hPutStr in_ (inBuffer s)
            modify $ \t -> t { inBuffer = "" } -}

keyboardMouseHandler hInWrite (Char c) Down modifiers position = do
    hPutChar hInWrite c
keyboardMouseHandler hInWrite chr st modifiers position = do return ()

main = do
    (pOutRead, pOutWrite) <- createPipe
    (pInRead, pInWrite) <- createPipe
    (pErrRead, pErrWrite) <- createPipe

    hInRead <- fdToHandle pInRead
    hInWrite <- fdToHandle pInWrite
    hOutRead <- fdToHandle pOutRead
    hOutWrite <- fdToHandle pOutWrite

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    hSetBuffering hInRead NoBuffering
    hSetBuffering hInWrite NoBuffering
    hSetBuffering hOutRead NoBuffering
    hSetBuffering hOutWrite NoBuffering

    initDisplay
    state <- makeState 

    checkGLSLSupport
    
    -- Initialize background shader
    backgroundPrg <- readCompileAndLink \
        "themes/default/background.vert"
        "themes/default/background.frag"
    backgroundProgram state $= Just backgroundPrg

    -- Initialize cursor shader
    cursorPrg <- readCompileAndLink \
        "themes/default/cursor.vert"
        "themes/default/cursor.frag"
    cursorProgram state $= Just cursorPrg

    displayCallback $= displayHandler state
    idleCallback $= Just (postRedisplay Nothing)
    keyboardMouseCallback $= Just (keyboardMouseHandler hInWrite)
    reshapeCallback $= Just (reshapeHandler)

    let environment = [
            ("TERM", "vt100"),
            ("COLUMS", "79"),
            ("ROWS", "24")]
        cmd = "script"
        cmdParams = ["-c", "bash --init-file .bashrc", "-f", "/dev/null"]
    process <- runProcess cmd cmdParams Nothing (Just environment)
            (Just hInRead) (Just hOutWrite) Nothing
    forkIO $ runTerminal (terminal state) hInWrite hOutRead
    mainLoop
