
module Graphics.Gloss.Internals.Interface.Display
        (displayWithBackend)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Rendering
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewState.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewState.Motion
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import qualified Graphics.Gloss.Internals.Interface.Callback as Callback
import Data.IORef
import System.Mem


displayWithBackend
        :: Backend a
        => a                            -- ^ Initial state of the backend.
        -> Display                      -- ^ Display config.
        -> Color                        -- ^ Background color.
        -> IO Picture                   -- ^ Make the picture to draw.
        -> (Controller -> IO ())        -- ^ Eat the controller
        -> IO ()

displayWithBackend
        backend displayMode background
        makePicture
        eatController

 =  do  viewSR          <- newIORef viewStateInit
        renderS         <- initState
        renderSR        <- newIORef renderS

        let renderFun backendRef = do
                port       <- viewStateViewPort <$> readIORef viewSR
                options    <- readIORef renderSR
                windowSize <- getWindowDimensions backendRef
                picture    <- makePicture

                displayPicture
                        windowSize
                        background
                        options
                        (viewPortScale port)
                        (applyViewPortToPicture port picture)

                -- perform GC every frame to try and avoid long pauses
                performGC

        let callbacks
             =  [ Callback.Display renderFun

                -- Escape exits the program
                , callback_exit ()

                -- Viewport control with mouse
                , callback_viewState_keyMouse viewSR
                , callback_viewState_motion   viewSR
                , callback_viewState_reshape ]

        -- When we create the window we can pass a function to get a
        -- reference to the backend state. Using this we make a controller
        -- so the client can control the window asynchronously.
        createWindow backend displayMode background callbacks
         $ \  backendRef
           -> eatController
                $ Controller
                { controllerSetRedraw
                   = do postRedisplay backendRef

                , controllerModifyViewPort
                   = \modViewPort
                     -> do viewState       <- readIORef viewSR
                           port'           <- modViewPort $ viewStateViewPort viewState
                           let viewState'  =  viewState { viewStateViewPort = port' }
                           writeIORef viewSR viewState'
                           postRedisplay backendRef
                }


