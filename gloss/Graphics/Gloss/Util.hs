module Graphics.Gloss.Util where

import Graphics.Gloss.Internals.Interface.Backend.GLUT

import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Data.IORef

-- | Get the size of the screen
screensize :: IO (Int,Int)
screensize = do
  backendStateRef <- newIORef glutStateInit
  initializeGLUT backendStateRef False
  GL.Size width height <- GLUT.get GLUT.screenSize
  return (fromIntegral width, fromIntegral height)
