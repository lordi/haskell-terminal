module Hsterm.State where
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float)

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
    cursorProgram :: IORef (Maybe Program)
}

makeState :: IO State
makeState = do
    terminal' <- newIORef defaultTerm

    startupTime' <- getCurrentTime >>= newIORef
    lastKeystrokeTime' <- getCurrentTime >>= newIORef

    backgroundProgram' <- newIORef Nothing
    foregroundProgram' <- newIORef Nothing
    cursorProgram' <- newIORef Nothing

    return State {
        terminal = terminal',

        startupTime = startupTime',
        lastKeystrokeTime = lastKeystrokeTime',

        backgroundProgram = backgroundProgram',
        foregroundProgram = foregroundProgram',
        cursorProgram = cursorProgram'
    }
