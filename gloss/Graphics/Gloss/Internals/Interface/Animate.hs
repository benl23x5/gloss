
module Graphics.Gloss.Internals.Interface.Animate
        (animateWithBackendIO)
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
import Graphics.Gloss.Internals.Interface.Animate.Timing
import qualified Graphics.Gloss.Internals.Interface.Animate.State       as AN
import qualified Graphics.Gloss.Internals.Interface.Callback            as Callback
import Data.IORef
import Control.Monad
import System.Mem
import GHC.Float (double2Float)


animateWithBackendIO
        :: Backend a
        => a                     -- ^ Initial State of the backend
        -> Bool                  -- ^ Whether to allow the image to be panned around.
        -> Display               -- ^ Display mode.
        -> Color                 -- ^ Background color.
        -> (Float -> IO Picture) -- ^ Function to produce the next frame of animation.
                                 --     It is passed the time in seconds since the program started.
        -> (Controller -> IO ()) -- ^ Eat the controller.
        -> IO ()

animateWithBackendIO
        backend pannable display backColor
        frameOp eatController
 = do
        --
        viewSR          <- newIORef viewStateInit
        animateSR       <- newIORef AN.stateInit
        renderS_        <- initState
        renderSR        <- newIORef renderS_

        let displayFun backendRef = do
                -- extract the current time from the state
                timeS           <- animateSR `getsIORef` AN.stateAnimateTime

                -- call the user action to get the animation frame
                picture         <- frameOp (double2Float timeS)

                renderS         <- readIORef renderSR
                portS           <- viewStateViewPort <$> readIORef viewSR

                windowSize      <- getWindowDimensions backendRef

                -- render the frame
                displayPicture
                        windowSize
                        backColor
                        renderS
                        (viewPortScale portS)
                        (applyViewPortToPicture portS picture)

                -- perform GC every frame to try and avoid long pauses
                performGC

        let callbacks
             =  [ Callback.Display      (animateBegin animateSR)
                , Callback.Display      displayFun
                , Callback.Display      (animateEnd   animateSR)
                , Callback.Idle         (\s -> postRedisplay s)
                , callback_exit ()
                , callback_viewState_motion viewSR
                , callback_viewState_reshape ]

             ++ (if pannable
                  then [callback_viewState_keyMouse viewSR]
                  else [])

        createWindow backend display backColor callbacks
           $ \ backendRef
           ->  eatController
                $ Controller
                { controllerSetRedraw
                   = postRedisplay backendRef

                , controllerModifyViewPort
                   = \modViewPort
                     -> do viewState       <- readIORef viewSR
                           port'           <- modViewPort $ viewStateViewPort viewState
                           let viewState'  =  viewState { viewStateViewPort = port' }
                           writeIORef viewSR viewState'
                           postRedisplay backendRef
                }





getsIORef :: IORef a -> (a -> r) -> IO r
getsIORef ref fun
 = liftM fun $ readIORef ref
