
module Graphics.Gloss.Internals.Interface.Display
	( displayInWindow
	, displayInWindowWithBackend)
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
import qualified Graphics.Gloss.Internals.Render.State	        		as RS
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Internals.Interface.Callback			as Callback

import Data.IORef

-- | Open a new window and display the given picture.
--
--   Use the following commands once the window is open:
--
-- 	* Quit - esc-key.
--
--	* Move Viewport - left-click drag, arrow keys.
--
--	* Rotate Viewport - right-click drag, control-left-click drag, or home\/end-keys.
--
--	* Zoom Viewport - mouse wheel, or page up\/down-keys.
--
displayInWindow
	:: String	-- ^ Name of the window.
	-> (Int, Int)	-- ^ Initial size of the window, in pixels.
	-> (Int, Int)	-- ^ Initial position of the window, in pixels.
	-> Color	-- ^ Background color.
	-> Picture	-- ^ The picture to draw.
	-> IO ()

displayInWindow
        = displayInWindowWithBackend defaultBackendState


displayInWindowWithBackend
	:: Backend a
	=> a		-- ^ Initial state of the backend.
	-> String	-- ^ Name of the window.
	-> (Int, Int)	-- ^ Initial size of the window, in pixels.
	-> (Int, Int)	-- ^ Initial position of the window, in pixels.
	-> Color	-- ^ Background color.
	-> Picture	-- ^ The picture to draw.
	-> IO ()

displayInWindowWithBackend backend name size pos background picture
 =  do	viewSR		<- newIORef viewPortInit
	viewControlSR	<- newIORef VPC.stateInit

        renderS         <- RS.stateInit
	renderSR	<- newIORef renderS
	
	let renderFun backendRef = do
		view	<- readIORef viewSR
		options	<- readIORef renderSR
	 	withViewPort
			backendRef
	 		view
			(renderPicture backendRef options view picture)

	let callbacks
	     =	[ Callback.Display renderFun 

		-- Delay the thread for a bit to give the runtime
		--	a chance to switch back to the OS.
		, Callback.Idle	   (\stateRef -> sleep stateRef 0.001 >> postRedisplay stateRef)

		-- Escape exits the program
		, callback_exit () 
		
		-- Viewport control with mouse
		, callback_viewPort_keyMouse viewSR viewControlSR 
		, callback_viewPort_motion   viewSR viewControlSR 
		, callback_viewPort_reshape ]

	createWindow backend name size pos background callbacks
