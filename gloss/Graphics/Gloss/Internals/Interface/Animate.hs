
module Graphics.Gloss.Internals.Interface.Animate
	( animateInWindow
	, animateInWindowWithBackend)
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
import qualified Graphics.Gloss.Internals.Render.State	        		as RS
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Internals.Interface.Animate.State		as AN
import qualified Graphics.Gloss.Internals.Interface.Callback			as Callback

import Data.IORef
import Control.Monad
import System.Mem
import GHC.Float (double2Float)

-- | Open a new window and display the given animation.
--
--   Once the window is open you can use the same commands as with @displayInWindow@.
--
animateInWindow
	:: String		-- ^ Name of the window.
	-> (Int, Int)		-- ^ Initial size of the window, in pixels.
	-> (Int, Int)		-- ^ Initial position of the window, in pixels.
	-> Color		-- ^ Background color.
	-> (Float -> Picture)	-- ^ Function to produce the next frame of animation. 
				--	It is passed the time in seconds since the program started.
	-> IO ()

animateInWindow
        = animateInWindowWithBackend defaultBackendState


animateInWindowWithBackend
	:: Backend a
	=> a			-- ^ Initial State of the backend
	-> String		-- ^ Name of the window.
	-> (Int, Int)		-- ^ Initial size of the window, in pixels.
	-> (Int, Int)		-- ^ Initial position of the window, in pixels.
	-> Color		-- ^ Background color.
	-> (Float -> Picture)	-- ^ Function to produce the next frame of animation.
				--	It is passed the time in seconds since the program started.
	-> IO ()

animateInWindowWithBackend backend name size pos backColor frameFun
 = do	
	viewSR		<- newIORef viewPortInit
	viewControlSR	<- newIORef VPC.stateInit
	animateSR	<- newIORef AN.stateInit
        renderS_        <- RS.stateInit
	renderSR	<- newIORef renderS_

 	let displayFun backendRef = do
		-- extract the current time from the state
  	 	timeS		<- animateSR `getsIORef` AN.stateAnimateTime

		-- call the user function to get the animation frame
		let picture	= frameFun (double2Float timeS)

		renderS		<- readIORef renderSR
		viewS		<- readIORef viewSR

		-- render the frame
		withViewPort
			backendRef
			viewS
			(renderPicture backendRef renderS viewS picture)

		-- perform GC every frame to try and avoid long pauses
		performGC

	let callbacks
	     = 	[ Callback.Display	(animateBegin animateSR)
		, Callback.Display 	displayFun
		, Callback.Display	(animateEnd   animateSR)
		, Callback.Idle		(\s -> postRedisplay s)
		, callback_exit () 
		, callback_viewPort_keyMouse viewSR viewControlSR 
		, callback_viewPort_motion   viewSR viewControlSR 
		, callback_viewPort_reshape ]

	createWindow backend name size pos backColor callbacks

getsIORef :: IORef a -> (a -> r) -> IO r
getsIORef ref fun
 = liftM fun $ readIORef ref
