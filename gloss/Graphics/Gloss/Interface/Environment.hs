module Graphics.Gloss.Interface.Environment where
import Graphics.Gloss.Internals.Interface.Backend.GLUT
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Data.IORef


-- | Get the size of the screen, in pixels.
--
--   This will be the size of the rendered gloss image when
--   fullscreen mode is enabled.
--
getScreenSize :: IO (Int, Int)
getScreenSize
 = do   backendStateRef         <- newIORef glutStateInit
        initializeGLUT backendStateRef False
        GL.Size width height    <- GLUT.get GLUT.screenSize
        return (fromIntegral width, fromIntegral height)

