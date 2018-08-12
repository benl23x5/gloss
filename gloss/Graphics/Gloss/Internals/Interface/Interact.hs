{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Internals.Interface.Interact
        (interactWithBackend)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Rendering
import Graphics.Gloss.Internals.Interface.Event
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import qualified Graphics.Gloss.Internals.Interface.Callback as Callback
import Data.IORef
import System.Mem


interactWithBackend
        :: Backend a
        => a                            -- ^ Initial state of the backend.
        -> Display                      -- ^ Display config.
        -> Color                        -- ^ Background color.
        -> world                        -- ^ The initial world.
        -> (world -> IO Picture)        -- ^ A function to produce the current picture.
        -> (Event -> world -> IO world) -- ^ A function to handle input events.
        -> (Controller -> IO ())        -- ^ Eat the controller
        -> IO ()

interactWithBackend
        backend displayMode background
        worldStart
        worldToPicture
        worldHandleEvent
        eatController

 =  do  viewSR          <- newIORef viewStateInit
        worldSR         <- newIORef worldStart
        renderS         <- initState
        renderSR        <- newIORef renderS

        let displayFun backendRef = do
                world      <- readIORef worldSR
                picture    <- worldToPicture world

                renderS'      <- readIORef renderSR
                viewState     <- readIORef viewSR
                let viewPort  =  viewStateViewPort viewState

                windowSize <- getWindowDimensions backendRef

                displayPicture
                        windowSize
                        background
                        renderS'
                        (viewPortScale viewPort)
                        (applyViewPortToPicture viewPort picture)

                -- perform GC every frame to try and avoid long pauses
                performGC

        let callbacks
             =  [ Callback.Display displayFun

                -- Viewport control with mouse
                , callback_keyMouse worldSR viewSR worldHandleEvent
                , callback_motion   worldSR worldHandleEvent
                , callback_reshape  worldSR worldHandleEvent ]

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


-- | Callback for KeyMouse events.
callback_keyMouse
        :: IORef world                  -- ^ ref to world state
        -> IORef ViewState
        -> (Event -> world -> IO world) -- ^ fn to handle input events
        -> Callback

callback_keyMouse worldRef viewRef eventFn
        = KeyMouse (handle_keyMouse worldRef viewRef eventFn)


handle_keyMouse
        :: IORef a
        -> t
        -> (Event -> a -> IO a)
        -> KeyboardMouseCallback

handle_keyMouse worldRef _ eventFn backendRef key keyState keyMods pos
 = do   ev         <- keyMouseEvent backendRef key keyState keyMods pos
        world      <- readIORef worldRef
        world'     <- eventFn ev world
        writeIORef worldRef world'
        postRedisplay backendRef


-- | Callback for Motion events.
callback_motion
        :: IORef world                  -- ^ ref to world state
        -> (Event -> world -> IO world) -- ^ fn to handle input events
        -> Callback

callback_motion worldRef eventFn
        = Motion (handle_motion worldRef eventFn)


handle_motion
        :: IORef a
        -> (Event -> a -> IO a)
        -> MotionCallback

handle_motion worldRef eventFn backendRef pos
 = do   ev       <- motionEvent backendRef pos
        world    <- readIORef worldRef
        world'   <- eventFn ev world
        writeIORef worldRef world'
        postRedisplay backendRef


-- | Callback for Handle reshape event.
callback_reshape
        :: IORef world
        -> (Event -> world -> IO world)
        -> Callback

callback_reshape worldRef eventFN
        = Reshape (handle_reshape worldRef eventFN)


handle_reshape
        :: IORef world
        -> (Event -> world -> IO world)
        -> ReshapeCallback
handle_reshape worldRef eventFn backendRef (width,height)
 = do   world  <- readIORef worldRef
        world' <- eventFn (EventResize (width, height)) world
        writeIORef worldRef world'
        viewState_reshape backendRef (width, height)
        postRedisplay backendRef

