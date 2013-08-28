{-# LANGUAGE Rank2Types #-}
module Main where
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

import Graphics.UI.GLUT hiding (Bool, Float, fontWidth, fontHeight)
import Graphics.Rendering.OpenGL hiding (Bool, Float, get)
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import Graphics.Rendering.FTGL (createTextureFont, renderFont, setFontFaceSize, RenderMode(..), getFontAdvance)
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
pathTerminalFont = "data/FreeMono.ttf" 
pathShellInit = "data/init.sh"
numColumns = 80
numRows = 24
fontSize = 20

cursorColor :: Color3 GLfloat 
cursorColor = Color3 1 1 1

backgroundColor :: Color4 GLfloat
backgroundColor = Color4 0.2 0.2 0.2 1

ansiColorToColor3 :: TerminalColor -> Color3 GLfloat
ansiColorToColor3 = fromJust . flip lookup ct
        where ct = [ (T.Black, Color3 0 0 0) 
                   , (T.Red, Color3 1 0 0)
                   , (T.Green, Color3 0 1 0)
                   , (T.Yellow, Color3 0 1 1)
                   , (T.Blue, Color3 0 0 1)
                   , (T.Magenta, Color3 1 1 0)
                   , (T.Cyan, Color3 1 0 1)
                   , (T.White, Color3 1 1 1) ] 
                   :: [(TerminalColor, Color3 GLfloat)]

initDisplay = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  createWindow "Haskell terminal emulator"
  clearColor $= backgroundColor

unitQuad = do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 (-0) (-0)   0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (-0)   1    0 )
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1    1    0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1  (-0)   0 )

reshapeHandler state size@(Size w h) = do
  ss <- getScreenSize state
  viewport $= (Position 0 0, ss)
  matrixMode $= Projection
  loadIdentity
  ortho (0::GLdouble) (fromIntegral w) 0 (fromIntegral h) 0 1

displayHandler :: State -> IO ()
displayHandler state = do
  term <- get $ terminal state

  clear [ColorBuffer]
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  matrixMode $= Modelview 0
  loadIdentity

  Just font <- get $ currentFont state
  fontHeight' <- get $ fontHeight state
  fontWidth' <- get $ fontWidth state

  let fontWidth'' = realToFrac fontWidth'
      fontHeight'' = realToFrac fontHeight'
      toScreenCoordinates :: Int -> Int -> Vector3 GLfloat
      toScreenCoordinates x y = Vector3 sx sy sz
        where sx = fontWidth'' * (fromIntegral x - 1)
              sy = fontHeight'' * fromIntegral (numRows - y)
              sz = 0 :: GLfloat
      blendQuad =
        preservingMatrix (scale fontWidth'' fontHeight'' 1.0 >> unitQuad)

  forM_ (indices $ screen term) $ \idx@(y, x) ->
    preservingMatrix $ do
        translate $ toScreenCoordinates x y

        -- Render a quad in the background color
        color $ ansiColorToColor3 (toEnum (background term ! idx))
        blendQuad

        -- Render a font in the foreground color
        color $ ansiColorToColor3 (toEnum (foreground term ! idx))
        renderFont font [screen term ! idx] All

  -- Cursor
  let (cy, cx) = cursorPos term
  translate $ toScreenCoordinates cx cy
  color cursorColor
  blendQuad

  swapBuffers

runTerminal :: IORef Terminal -> Handle -> Handle -> IO ()
runTerminal a in_ out =
    forever $ do
        c <- (liftIO $ hGetChar out)
        s <- readIORef a

        -- Parse the input buffer for characters or ANSI sequences
        Right (actions, leftover) <- return $ parseANSI $ inBuffer s ++ [c]

        -- Apply all the actions to the terminal state
        forM_ actions $ \x -> modifyIORef a $ applyAction x

        -- Store the actions that could not be parsed as input buffer
        modifyIORef a $ \term -> term { inBuffer = leftover }

keyboardMouseHandler hInWrite (Char c) Down modifiers position = 
    hPutChar hInWrite c
keyboardMouseHandler hInWrite chr st modifiers position = return ()

getScreenSize :: State -> IO Size
getScreenSize state = do
    fontWidth' <- get $ fontWidth state
    fontHeight' <- get $ fontHeight state
    return (Size (round (fontWidth' * fromIntegral numColumns)) (round (fontHeight' * fromIntegral numRows)))

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

    font <- createTextureFont pathTerminalFont
    setFontFaceSize font (fromIntegral fontSize) 144
    currentFont state $= Just font
    advance <- getFontAdvance font "_"
    fontWidth state $= advance
    fontHeight state $= fromIntegral fontSize

    ss <- getScreenSize state
    windowSize $= ss
    {-
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
    -}

    displayCallback $= displayHandler state
    idleCallback $= Just (postRedisplay Nothing)
    keyboardMouseCallback $= Just (keyboardMouseHandler hInWrite)
    reshapeCallback $= Just (reshapeHandler state)

    let environment = [
            ("TERM", "xterm"),
            ("COLUMS", "79"),
            ("ROWS", "24")]
        cmd = "script"
        cmdParams = ["-c", "bash --init-file " ++ pathShellInit, "-f", "/dev/null"]
    process <- runProcess cmd cmdParams Nothing (Just environment)
            (Just hInRead) (Just hOutWrite) Nothing
    forkIO $ runTerminal (terminal state) hInWrite hOutRead
    mainLoop
