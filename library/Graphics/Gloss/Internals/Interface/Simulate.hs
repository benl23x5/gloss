{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Internals.Interface.Simulate
	( simulateInWindow
	, simulateInWindowWithBackend)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Render.ViewPort
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewPort
import Graphics.Gloss.Internals.Interface.ViewPort.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewPort.Motion
import Graphics.Gloss.Internals.Interface.ViewPort.Reshape
import Graphics.Gloss.Internals.Interface.Animate.Timing
import Graphics.Gloss.Internals.Interface.Simulate.Idle
import qualified Graphics.Gloss.Internals.Interface.Callback			as Callback
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Internals.Interface.Simulate.State		as SM
import qualified Graphics.Gloss.Internals.Interface.Animate.State		as AN
import qualified Graphics.Gloss.Internals.Render.State	        		as RS
import Data.IORef
import System.Mem

-- | Run a finite-time-step simulation in a window. You decide how the model is represented,
--	how to convert the model to a picture, and how to advance the model for each unit of time. 
--	This function does the rest.
--
--   Once the window is open you can use the same commands as with @displayInWindow@.
--
simulateInWindow 
	:: forall model
	.  String			-- ^ Name of the window.
	-> (Int, Int)			-- ^ Initial size of the window, in pixels.
	-> (Int, Int)			-- ^ Initial position of the window, in pixels.
	-> Color			-- ^ Background color.
	-> Int				-- ^ Number of simulation steps to take for each second of real time.
	-> model 			-- ^ The initial model.
	-> (model -> Picture)	 	-- ^ A function to convert the model to a picture.
	-> (ViewPort -> Float -> model -> model) -- ^ A function to step the model one iteration. It is passed the 
						 --	current viewport and the amount of time for this simulation
						 --     step (in seconds).
	-> IO ()

simulateInWindow
        = simulateInWindowWithBackend defaultBackendState

simulateInWindowWithBackend
	:: forall model a
	.  Backend a
	=> a				-- ^ Initial state of the backend
	-> String			-- ^ Name of the window.
	-> (Int, Int)			-- ^ Initial size of the window, in pixels.
	-> (Int, Int)			-- ^ Initial position of the window, in pixels.
	-> Color			-- ^ Background color.
	-> Int				-- ^ Number of simulation steps to take for each second of real time.
	-> model 			-- ^ The initial model.
	-> (model -> Picture)	 	-- ^ A function to convert the model to a picture.
	-> (ViewPort -> Float -> model -> model) -- ^ A function to step the model one iteration. It is passed the
						 --	current viewport and the amount of time for this simulation
						 --     step (in seconds).
	-> IO ()

simulateInWindowWithBackend
	backend
	windowName
	windowSize
	windowPos
	backgroundColor
	simResolution
	worldStart
	worldToPicture
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
						worldSR worldStart worldAdvance
						singleStepTime)
		, callback_exit () 
		, callback_viewPort_keyMouse viewSR viewControlSR 
		, callback_viewPort_motion   viewSR viewControlSR 
		, callback_viewPort_reshape ]

	createWindow backend windowName windowSize windowPos backgroundColor callbacks
