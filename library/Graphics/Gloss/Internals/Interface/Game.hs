{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.Game
	( gameInWindow
	, Event(..))
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.ViewPort
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Render.ViewPort
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Callback
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewPort.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewPort.Motion
import Graphics.Gloss.Internals.Interface.ViewPort.Reshape
import Graphics.Gloss.Internals.Interface.Animate.Timing
import Graphics.Gloss.Internals.Interface.Simulate.Idle
import qualified Graphics.Gloss.Internals.Interface.Callback			as Callback
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Internals.Interface.Simulate.State		as SM
import qualified Graphics.Gloss.Internals.Interface.Animate.State		as AN
import qualified Graphics.Gloss.Internals.Render.Options			as RO
import qualified Graphics.UI.GLUT						as GLUT
import qualified Graphics.Rendering.OpenGL.GL					as GL
import Data.IORef
import System.Mem

-- | Possible input events.
data Event
	= EventKey    GLUT.Key GLUT.KeyState GLUT.Modifiers (Float, Float)
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
	viewControlSR	<- newIORef VPC.stateInit
	renderSR	<- newIORef RO.optionsInit
	animateSR	<- newIORef AN.stateInit

	let displayFun
	     = do
		-- convert the world to a picture
		world		<- readIORef worldSR
		let picture	= worldToPicture world
	
		-- display the picture in the current view
		renderS		<- readIORef renderSR
		viewS		<- readIORef viewSR

		-- render the frame
		withViewPort 
			viewS
	 	 	(renderPicture renderS viewS picture)
 
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

	createWindow windowName windowSize windowPos backgroundColor callbacks


-- | Callback for KeyMouse events.
callback_keyMouse 
	:: IORef world	 		-- ^ ref to world state
	-> IORef ViewPort
	-> (Event -> world -> world)	-- ^ fn to handle input events
	-> Callback

callback_keyMouse worldRef viewRef eventFn
 	= KeyMouse (handle_keyMouse worldRef viewRef eventFn)

handle_keyMouse worldRef viewRef eventFn key keyState keyMods pos
 = do	size@(GLUT.Size sizeX_ sizeY_)	<- GL.get GLUT.windowSize
	let (sizeX, sizeY)		= (fromIntegral sizeX_, fromIntegral sizeY_)

	let GLUT.Position px_ py_	= pos
	let px		= fromIntegral px_
	let py		= sizeY - fromIntegral py_
	
	let px'		= px - sizeX / 2
	let py' 	= py - sizeY / 2
	let pos'	= (px', py')
  
	worldRef `modifyIORef` \world -> eventFn (EventKey key keyState keyMods pos') world


-- | Callback for Motion events.
callback_motion
	:: IORef world	 		-- ^ ref to world state
	-> (Event -> world -> world)	-- ^ fn to handle input events
	-> Callback

callback_motion worldRef eventFn
 	= Motion (handle_motion worldRef eventFn)

handle_motion worldRef eventFn pos
 = do	size@(GLUT.Size sizeX_ sizeY_)	<- GL.get GLUT.windowSize
	let (sizeX, sizeY)		= (fromIntegral sizeX_, fromIntegral sizeY_)
	
	let GLUT.Position px_ py_	= pos
	let px		= fromIntegral px_
	let py		= sizeY - fromIntegral py_
	
	let px'		= px - sizeX / 2
	let py' 	= py - sizeY / 2
	let pos'	= (px', py')
  
 	worldRef `modifyIORef` \world -> eventFn (EventMotion pos') world







