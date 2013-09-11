{-# LANGUAGE Rank2Types #-}
module Hsterm.Hsterm where
import System.Process
import Data.Array.Diff
import Data.IORef
import Data.Char
import Data.Maybe (fromJust)
-- import Control.Monad
import Control.Monad.State hiding (state, get, State)
import System.IO

import Data.Time.Clock
import Data.Time.Calendar
import Data.Colour.SRGB (RGB(..), toSRGB)
import Data.Colour (Colour(..))

import Graphics.UI.GLUT hiding (Bool, Float, fontWidth, fontHeight, RGB)
import Graphics.Rendering.OpenGL hiding (Bool, Float, get, RGB)
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
import Hsterm.Config

-- Constants (for now)
numColumns = 80
numRows = 24

initDisplay = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  createWindow "Haskell terminal emulator"
  -- TODO  clearColor $= backgroundColor

glColor :: Colour Double -> Color3 GLfloat
glColor c = Color3 (realToFrac r) (realToFrac g) (realToFrac b)
    where RGB r g b = toSRGB c

unitQuad = do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 (-0) (-0)   0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (-0)   1    0 )
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1    1    0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1  (-0)   0 )

reshapeHandler state size@(Size w_ h_) = do
  ss@(Size w h) <- getScreenSize state
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
  cfg <- get $ config state


  let fontWidth'' = realToFrac fontWidth'
      fontHeight'' = realToFrac fontHeight'
      setColor :: Bool -> TerminalColor -> IO ()
      setColor bright c = do color (glColor (cm cfg c))
                        where cm = if bright then colorMapBright else colorMap
      toScreenCoordinates :: Int -> Int -> Vector3 GLfloat
      toScreenCoordinates x y = Vector3 sx sy sz
        where sx = fontWidth'' * (fromIntegral x - 1)
              sy = fontHeight'' * fromIntegral (numRows - y)
              sz = 0 :: GLfloat
      blendQuad =
        preservingMatrix (scale fontWidth'' fontHeight'' 1.0 >> unitQuad)

  forM_ (indices $ screen term) $ \idx@(y, x) ->
    preservingMatrix $ do
        let tc = screen term ! idx
        translate $ toScreenCoordinates x y

        -- Render a quad in the background color
        setColor False (backgroundColor tc)
        blendQuad

        -- Render a font in the foreground color
        setColor (isBright tc) (foregroundColor tc)
        translate $ Vector3 0 (4) (0 :: GLfloat)
        renderFont font [character tc] All

  -- Cursor
  case (optionShowCursor term) of
    True -> do
      let (y, x) = cursorPos term
      translate $ toScreenCoordinates x y
      color $ glColor $ cursorColor cfg
      blendQuad
    _ -> return ()

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

runHsterm :: TerminalConfig -> IO ()
runHsterm cfg = do
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
    state <- makeState cfg

    font <- createTextureFont (fontPath cfg)
    setFontFaceSize font (fromIntegral (fontSize cfg)) 144
    currentFont state $= Just font
    advance <- getFontAdvance font "_"
    fontWidth state $= advance
    fontHeight state $= fromIntegral (fontSize cfg)

    ss <- getScreenSize state
    windowSize $= ss

    displayCallback $= displayHandler state
    idleCallback $= Just (postRedisplay Nothing)
    keyboardMouseCallback $= Just (keyboardMouseHandler hInWrite)
    reshapeCallback $= Just (reshapeHandler state)

    let environment = [
            ("TERM", "xterm"),
            ("COLUMS", "79"),
            ("ROWS", "24")]
        cmd = "script"
        cmdParams = ["-c", "bash --init-file " ++ (initScriptPath cfg), "-f", "/dev/null"]
    process <- runProcess cmd cmdParams Nothing (Just environment)
            (Just hInRead) (Just hOutWrite) Nothing
    forkIO $ runTerminal (terminal state) hInWrite hOutRead
    mainLoop
