{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Internals.Interface.Simulate
        (simulateWithBackendIO)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
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
import Graphics.Gloss.Internals.Interface.Simulate.Idle
import qualified Graphics.Gloss.Internals.Interface.Callback            as Callback
import qualified Graphics.Gloss.Internals.Interface.Simulate.State      as SM
import qualified Graphics.Gloss.Internals.Interface.Animate.State       as AN
import Data.IORef
import System.Mem


simulateWithBackendIO
        :: forall model a
        .  Backend a
        => a            -- ^ Initial state of the backend
        -> Display      -- ^ Display mode.
        -> Color        -- ^ Background color.
        -> Int          -- ^ Number of simulation steps to take for each second of real time.
        -> model        -- ^ The initial model.
        -> (model -> IO Picture)
                -- ^ A function to convert the model to a picture.
        -> (ViewPort -> Float -> model -> IO model)
                -- ^ A function to step the model one iteration. It is passed the
                --     current viewport and the amount of time for this simulation
                --     step (in seconds).
        -> IO ()

simulateWithBackendIO
        backend
        display
        backgroundColor
        simResolution
        worldStart
        worldToPicture
        worldAdvance
 = do
        let singleStepTime      = 1

        -- make the simulation state
        stateSR         <- newIORef $ SM.stateInit simResolution

        -- make a reference to the initial world
        worldSR         <- newIORef worldStart

        -- make the initial GL view and render states
        viewSR          <- newIORef viewStateInit
        animateSR       <- newIORef AN.stateInit
        renderS_        <- initState
        renderSR        <- newIORef renderS_

        let displayFun backendRef
             = do
                -- convert the world to a picture
                world           <- readIORef worldSR
                port            <- viewStateViewPort <$> readIORef viewSR
                picture         <- worldToPicture world

                -- display the picture in the current view
                renderS         <- readIORef renderSR

                windowSize      <- getWindowDimensions backendRef

                -- render the frame
                displayPicture
                        windowSize
                        backgroundColor
                        renderS
                        (viewPortScale port)
                        (applyViewPortToPicture port picture)

                -- perform GC every frame to try and avoid long pauses
                performGC

        let callbacks
             =  [ Callback.Display      (animateBegin animateSR)
                , Callback.Display      displayFun
                , Callback.Display      (animateEnd   animateSR)
                , Callback.Idle         (callback_simulate_idle
                                                stateSR animateSR
                                                (viewStateViewPort <$> readIORef viewSR)
                                                worldSR worldAdvance
                                                singleStepTime)
                , callback_exit ()
                , callback_viewState_keyMouse viewSR
                , callback_viewState_motion   viewSR
                , callback_viewState_reshape ]

        createWindow backend display backgroundColor
                callbacks
                (const (return ()))


