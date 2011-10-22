{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Internals.Interface.Game
	( gameInWindow
	, gameInWindowWithBackend
	, Event(..))
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Render.ViewPort
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewPort
import Graphics.Gloss.Internals.Interface.ViewPort.Reshape
import Graphics.Gloss.Internals.Interface.Animate.Timing
import Graphics.Gloss.Internals.Interface.Simulate.Idle
import qualified Graphics.Gloss.Internals.Interface.Callback		as Callback
import qualified Graphics.Gloss.Internals.Interface.Simulate.State	as SM
import qualified Graphics.Gloss.Internals.Interface.Animate.State	as AN
import qualified Graphics.Gloss.Internals.Render.State	        	as RS
import Data.IORef
import System.Mem

-- | Possible input events.
data Event
	= EventKey    Key KeyState Modifiers (Float, Float)
	| EventMotion (Float, Float)
	deriving (Eq, Show)

-- | Run a game in a window. 
gameInWindow 
	:: forall world
	.  String			-- ^ Name of the window.
	-> (Int, Int)			-- ^ Initial size of the window, in pixels.
	-> (Int, Int)			-- ^ Initial position of the window, in pixels.
	-> Color			-- ^ Background color.
	-> Int				-- ^ Number of simulation steps to take for each second of real time.
	-> world 			-- ^ The initial world.
	-> (world -> Picture)	 	-- ^ A function to convert the world a picture.
	-> (Event -> world -> world)	-- ^ A function to handle input events.
	-> (Float -> world -> world)   	-- ^ A function to step the world one iteration.
					--   It is passed the period of time (in seconds) needing to be advanced.
	-> IO ()

gameInWindow
        = gameInWindowWithBackend defaultBackendState

gameInWindowWithBackend
	:: forall world a
	.  Backend a
	=> a				-- ^ Initial state of the backend
	-> String			-- ^ Name of the window.
	-> (Int, Int)			-- ^ Initial size of the window, in pixels.
	-> (Int, Int)			-- ^ Initial position of the window, in pixels.
	-> Color			-- ^ Background color.
	-> Int				-- ^ Number of simulation steps to take for each second of real time.
	-> world 			-- ^ The initial world.
	-> (world -> Picture)	 	-- ^ A function to convert the world a picture.
	-> (Event -> world -> world)	-- ^ A function to handle input events.
	-> (Float -> world -> world)   	-- ^ A function to step the world one iteration.
					--   It is passed the period of time (in seconds) needing to be advanced.
	-> IO ()

gameInWindowWithBackend
	backend
	windowName
	windowSize
	windowPos
	backgroundColor
	simResolution
	worldStart
	worldToPicture
	worldHandleEvent
	worldAdvance
 = do
	let singleStepTime	= 1

	-- make the simulation state
	stateSR		<- newIORef $ SM.stateInit simResolution

	-- make a reference to the initial world
	worldSR		<- newIORef worldStart

	-- make the initial GL view and render states
	viewSR		<- newIORef viewPortInit
	animateSR	<- newIORef AN.stateInit
        renderS_        <- RS.stateInit
	renderSR	<- newIORef renderS_

	let displayFun backendRef
	     = do
		-- convert the world to a picture
		world		<- readIORef worldSR
		let picture	= worldToPicture world
	
		-- display the picture in the current view
		renderS		<- readIORef renderSR
		viewS		<- readIORef viewSR

		-- render the frame
		withViewPort
			backendRef
			viewS
	 	 	(renderPicture backendRef renderS viewS picture)
 
		-- perform garbage collection
		performGC

	let callbacks
	     = 	[ Callback.Display	(animateBegin animateSR)
		, Callback.Display 	displayFun
		, Callback.Display	(animateEnd   animateSR)
		, Callback.Idle		(callback_simulate_idle 
						stateSR animateSR viewSR 
						worldSR worldStart (\_ -> worldAdvance)
						singleStepTime)
		, callback_exit () 
		, callback_keyMouse worldSR viewSR worldHandleEvent
		, callback_motion   worldSR worldHandleEvent
		, callback_viewPort_reshape ]

	createWindow backend windowName windowSize windowPos backgroundColor callbacks


-- | Callback for KeyMouse events.
callback_keyMouse 
	:: IORef world	 		-- ^ ref to world state
	-> IORef ViewPort
	-> (Event -> world -> world)	-- ^ fn to handle input events
	-> Callback

callback_keyMouse worldRef viewRef eventFn
 	= KeyMouse (handle_keyMouse worldRef viewRef eventFn)


handle_keyMouse 
	:: IORef a
	-> t
	-> (Event -> a -> a)
	-> KeyboardMouseCallback

handle_keyMouse worldRef _ eventFn backendRef key keyState keyMods pos
 = do	pos' <- convertPoint backendRef pos
	worldRef `modifyIORef` \world -> eventFn (EventKey key keyState keyMods pos') world


-- | Callback for Motion events.
callback_motion
	:: IORef world	 		-- ^ ref to world state
	-> (Event -> world -> world)	-- ^ fn to handle input events
	-> Callback

callback_motion worldRef eventFn
 	= Motion (handle_motion worldRef eventFn)


handle_motion 
	:: IORef a
	-> (Event -> a -> a)
	-> MotionCallback

handle_motion worldRef eventFn backendRef pos
 = do pos' <- convertPoint backendRef pos
      worldRef `modifyIORef` \world -> eventFn (EventMotion pos') world


convertPoint ::
	forall a . Backend a
	=> IORef a
	-> (Int, Int)
	-> IO (Float,Float)
convertPoint backendRef pos
 = do	(sizeX_, sizeY_) 		<- getWindowDimensions backendRef
	let (sizeX, sizeY)		= (fromIntegral sizeX_, fromIntegral sizeY_)

	let (px_, py_)	= pos
	let px		= fromIntegral px_
	let py		= sizeY - fromIntegral py_
	
	let px'		= px - sizeX / 2
	let py' 	= py - sizeY / 2
	let pos'	= (px', py')
	return pos'

