-- Utils for loading, compiling and linking GLSL shaders
-- Largely taken from GLUT's Brick.hs example
module Hsterm.ShaderUtils where

import Prelude hiding ( sum )
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Foldable ( Foldable, sum )
import Data.IORef
import Graphics.UI.GLUT

-- Make sure that GLSL is supported by the driver, either directly by the core
-- or via an extension.
checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

readAndCompileShader :: Shader s => FilePath -> IO s
readAndCompileShader filePath = do
   src <- readFile filePath
   [shader] <- genObjectNames 1
   shaderSource shader $= [src]
   compileShader shader
   reportErrors
   ok <- get (compileStatus shader)
   infoLog <- get (shaderInfoLog shader)
   unless ok $ do
      mapM_ putStrLn ["Notice: Loaded shader '" ++ filePath ++ "': " ++ infoLog]
      deleteObjectNames [shader]
      ioError (userError "shader compilation failed")
   return shader

linkShaders :: [VertexShader] -> [FragmentShader] -> IO (Program)
linkShaders vs fs = do
   [prog] <- genObjectNames 1
   attachedShaders prog $= (vs, fs)
   linkProgram prog
   reportErrors
   ok <- get (linkStatus prog)
   unless ok $ do
      infoLog <- get (programInfoLog prog)
      putStrLn infoLog
      deleteObjectNames [prog]
      ioError (userError "linking failed")
   return prog

readCompileAndLink :: String -> String -> IO (Program)
readCompileAndLink vspath fspath = do
  vs <- readAndCompileShader vspath
  fs <- readAndCompileShader fspath
  linkShaders [vs] [fs]

