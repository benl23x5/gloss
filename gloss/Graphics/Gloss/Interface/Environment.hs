module Graphics.Gloss.Interface.Environment where

import Data.IORef (newIORef)

import qualified Graphics.Gloss.Internals.Interface.Backend.Types as Backend.Types
import Graphics.Gloss.Internals.Interface.Backend (defaultBackendState)

-- | Get the size of the screen, in pixels.
--
--   This will be the size of the rendered gloss image when
--   fullscreen mode is enabled.
--
getScreenSize :: IO (Int, Int)
getScreenSize = do
       backendStateRef <- newIORef defaultBackendState
       Backend.Types.initializeBackend backendStateRef False
       Backend.Types.getScreenSize backendStateRef

