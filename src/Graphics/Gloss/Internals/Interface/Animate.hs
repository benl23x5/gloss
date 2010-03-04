{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.Animate
	(animateInWindow)
where	
import Graphics.Gloss.Color
import Graphics.Gloss.Picture
import Graphics.Gloss.ViewPort
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Render.ViewPort
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewPort.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewPort.Motion
import Graphics.Gloss.Internals.Interface.ViewPort.Reshape
import Graphics.Gloss.Internals.Interface.Animate.Timing
import qualified Graphics.Gloss.Internals.Render.Options			as RO
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Internals.Interface.Animate.State		as AN
import qualified Graphics.Gloss.Internals.Interface.Callback			as Callback

import qualified Graphics.UI.GLUT				as GLUT
import Data.IORef
import Control.Monad
import System.Mem

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

animateInWindow name size pos backColor frameFun
 = do	
	viewSR		<- newIORef viewPortInit
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
