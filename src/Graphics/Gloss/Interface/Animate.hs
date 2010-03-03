module Graphics.Gloss.Interface.Animate
	(animateInWindow)
where	
import Graphics.Gloss.Color
import Graphics.Gloss.Picture
import Graphics.Gloss.Render.Picture
import Graphics.Gloss.Render.ViewPort
import Graphics.Gloss.Interface.Window
import Graphics.Gloss.Interface.Exit
import Graphics.Gloss.Interface.ViewPort.KeyMouse
import Graphics.Gloss.Interface.ViewPort.Motion
import Graphics.Gloss.Interface.ViewPort.Reshape
import Graphics.Gloss.Interface.Animate.Timing
import qualified Graphics.Gloss.Render.Options			as RO
import qualified Graphics.Gloss.Interface.ViewPort.State	as VP
import qualified Graphics.Gloss.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Interface.Animate.State		as AN
import qualified Graphics.Gloss.Interface.Callback		as Callback
import qualified Graphics.UI.GLUT				as GLUT
import Data.IORef
import Control.Monad
import System.Mem

-- | Open a window and display an animation.
animateInWindow
	:: String		-- ^ Name of the window.
	-> (Int, Int)		-- ^ Initial size of the window, in pixels.
	-> (Int, Int)		-- ^ Initial position of the window, in pixels.
	-> Color		-- ^ Background color.
	-> (Float -> Picture)	-- ^ New frame function. The function is passed the current animation time, in seconds.
	-> IO ()

animateInWindow name size pos backColor frameFun
 = do	
	viewSR		<- newIORef VP.stateInit
	viewControlSR	<- newIORef VPC.stateInit
	renderSR	<- newIORef RO.optionsInit
	animateSR	<- newIORef AN.stateInit

 	let displayFun = do
		-- extract the current time from the state
  	 	time		<- animateSR `getsIORef` AN.stateAnimateTime
		let timeS	= (fromIntegral time / 1000)

		-- call the user function to get the animation frame
		let picture	= frameFun timeS 

		renderS		<- readIORef renderSR
		viewS		<- readIORef viewSR

		-- render the frame
		withViewPort
			viewS
			(renderPicture renderS viewS picture)

		-- perform GC every frame to try and avoid long pauses
		performGC

	let callbacks
	     = 	[ Callback.Display	(animateBegin animateSR)
		, Callback.Display 	displayFun
		, Callback.Display	(animateEnd   animateSR)
		, Callback.Idle		(GLUT.postRedisplay Nothing)
		, callback_exit () 
		, callback_viewPort_keyMouse viewSR viewControlSR 
		, callback_viewPort_motion   viewSR viewControlSR 
		, callback_viewPort_reshape ]

	createWindow name size pos backColor callbacks


getsIORef ref fun
 = liftM fun $ readIORef ref
