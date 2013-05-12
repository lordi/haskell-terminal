{-# LANGUAGE Rank2Types #-}
import System.Process
import Data.Array.Unboxed
import Data.IORef
import Data.Char
import Control.Monad
import Control.Monad.State hiding (state, get)
import System.IO

import Graphics.UI.GLUT hiding (Bool, Float)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL hiding (Bool, Float, get)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix) -- TODO check if these are really needed
import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)
import GHC.IO.Handle
import Debug.Trace
import Control.Concurrent
import Control.Applicative hiding (many)
import Text.Parsec
import Text.Parsec.String

import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

initDisplay = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
  createWindow "Termskell"

  materialShininess Front $= 0.0
  shadeModel $= Smooth
  frontFace $= CW
  autoNormal $= Enabled
  normalize $= Enabled
  depthFunc $= Just Less
  -- cullFace $= Just Back

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  clearColor $= Color4 0 0 0 1

font = Fixed9By15

displayHandler a = do
  clear [ColorBuffer, DepthBuffer]
  term <- readIORef a

  let withTextMode sth = do
      textureBinding Texture2D $= Nothing
      matrixMode $= Modelview 0
      preservingMatrix $ do
          loadIdentity
          sth

  -- TODO prettify code
  (Size screenWidth screenHeight) <- get windowSize
  chrWidth <- stringWidth font " "
  chrHeight <- fontHeight font
  let (chrWidth', chrHeight') = ((fromIntegral chrWidth) * 2.0 / (fromIntegral screenWidth), chrHeight * 2.0 / (fromIntegral screenHeight))
  let rasterPosition (x, y) = Vertex4 (-1 + ((fromIntegral x - 1) * chrWidth')) (1 - (fromIntegral y) * chrHeight') 0 (1::GLfloat)

  withTextMode $ do
    currentRasterPosition $= Vertex4 (-1) (-0.95) 0 (1::GLfloat)
    renderString font $ show ((cursorPos term), inBuffer term)
    let lines = chunk (cols term) $ elems (screen term)
        (y, x) = cursorPos term
    forM_ (zip [1..] lines) $ \(i, s) -> do
        currentRasterPosition $= rasterPosition (1, i)
        renderString font s

    -- Show cursor
    currentRasterPosition $= rasterPosition (x, y)
    renderString font "|"

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
        Right (actions, leftover) <- return $ play $ (inBuffer s) ++ [c]

        -- Apply all the actions to the terminal state
        forM actions $ \x -> modifyIORef a $ applyAction x

        -- Store the actions that could not be parsed as input buffer
        modifyIORef a $ \term -> term { inBuffer = leftover }
        
{-      s <- get
        when ((inBuffer s) /= "") $ do
            liftIO $ putStrLn "writing sth"
            liftIO $ hPutStr in_ (inBuffer s)
            modify $ \t -> t { inBuffer = "" } -}

redirect :: Handle -> Handle -> IO ()
redirect from to =
    forever $ do
        hGetChar from >>= hPutChar to

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

    a <- newIORef defaultTerm

    displayCallback $= displayHandler a
    idleCallback $= Just (postRedisplay Nothing)
    keyboardMouseCallback $= Just (keyboardMouseHandler hInWrite)

    let environment = [
            ("TERM", "vt100"),
            ("COLUMS", "79"),
            ("ROWS", "24")]
    process <- runProcess "script" ["-c", "bash --init-file .bashrc", "-f", "/dev/null"] Nothing (Just environment)
            (Just hInRead) (Just hOutWrite) Nothing
    -- 
    -- forkIO $ redirect stdin hInWrite
    forkIO $ runTerminal a hInWrite hOutRead
    -- forkIO $ runStateT (runTerminal hInWrite hOutRead) (initTerm (24, 80)) >> return ()
    mainLoop
