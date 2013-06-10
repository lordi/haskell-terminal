module Hsterm.State where
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float, Font, fontHeight, fontWidth)
import Graphics.Rendering.FTGL (Font)

import Data.Time.Clock (getCurrentTime)
import qualified Data.Time.Clock as C

import Terminal.Terminal
import Terminal.Types

data State = State {
    terminal :: IORef Terminal,
    startupTime :: IORef C.UTCTime,
    lastKeystrokeTime :: IORef C.UTCTime,
    backgroundProgram :: IORef (Maybe Program),
    foregroundProgram :: IORef (Maybe Program),
    cursorProgram :: IORef (Maybe Program),
    currentFont :: IORef (Maybe Font),
    fontWidth :: IORef Float,
    fontHeight :: IORef Float
}

makeState :: IO State
makeState = do
    terminal' <- newIORef defaultTerm

    startupTime' <- getCurrentTime >>= newIORef
    lastKeystrokeTime' <- getCurrentTime >>= newIORef

    backgroundProgram' <- newIORef Nothing
    foregroundProgram' <- newIORef Nothing
    cursorProgram' <- newIORef Nothing
    currentFont' <- newIORef Nothing
    fontWidth' <- newIORef 0.0
    fontHeight' <- newIORef 0.0

    return State {
        terminal = terminal',

        startupTime = startupTime',
        lastKeystrokeTime = lastKeystrokeTime',

        backgroundProgram = backgroundProgram',
        foregroundProgram = foregroundProgram',
        cursorProgram = cursorProgram',
        currentFont = currentFont',
        fontHeight = fontHeight',
        fontWidth = fontWidth'
    }
