
module Graphics.Gloss.Interface.Simulate
	(simulateInWindow)
where
import Graphics.Gloss.Color
import Graphics.Gloss.Picture
import Graphics.Gloss.Render.Picture
import Graphics.Gloss.Render.ViewPort
import Graphics.Gloss.Interface.Exit
import Graphics.Gloss.Interface.Window
import Graphics.Gloss.Interface.ViewPort.KeyMouse
import Graphics.Gloss.Interface.ViewPort.Motion
import Graphics.Gloss.Interface.ViewPort.Reshape
import Graphics.Gloss.Interface.Animate.Timing
import Graphics.Gloss.Interface.Simulate.Idle
import qualified Graphics.Gloss.Interface.Callback		as Callback
import qualified Graphics.Gloss.Interface.ViewPort.State	as VP
import qualified Graphics.Gloss.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Interface.Simulate.State	as SM
import qualified Graphics.Gloss.Interface.Animate.State		as AN
import qualified Graphics.Gloss.Render.Options			as RO
import qualified Graphics.UI.GLUT				as GLUT
import Data.IORef
import System.Mem

-- | Run a simulation in a window
simulateInWindow
	:: String			-- ^ The name of the window.
	-> (Int, Int)			-- ^ Initial size of the window, in pixels.
	-> (Int, Int)			-- ^ Initial position of the window, in pixels.
	-> Color			-- ^ Background color.
	-> Int				-- ^ Number of steps to take for each second of real time.
	-> world 			-- ^ The initial world.
	-> (world -> Picture)	 	-- ^ A function to convert the world a picture.

	-> (VP.State -> Float -> world -> world) 	
					-- ^ A function to step the world one iteration.
					--	The float gives the simulation period (time step).
	-> Float			-- ^ How much time to advance the world by in single step mode.
	-> IO ()

simulateInWindow
	windowName
	windowSize
	windowPos
	backgroundColor
	simResolution
	worldStart
	worldToPicture
	worldAdvance
	singleStepTime
 = do
	-- make the simulation state
	stateSR		<- newIORef $ SM.stateInit simResolution

	-- make a reference to the initial world
	worldSR		<- newIORef worldStart

	-- make the initial GL view and render states
	viewSR		<- newIORef VP.stateInit
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
						worldSR worldStart worldAdvance
						singleStepTime)
		, callback_exit () 
		, callback_viewPort_keyMouse viewSR viewControlSR 
		, callback_viewPort_motion   viewSR viewControlSR 
		, callback_viewPort_reshape ]

	createWindow windowName windowSize windowPos backgroundColor callbacks
