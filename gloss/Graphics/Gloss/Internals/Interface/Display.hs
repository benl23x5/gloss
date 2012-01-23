
module Graphics.Gloss.Internals.Interface.Display
	(displayWithBackend)
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


displayWithBackend
	:: Backend a
	=> a                -- ^ Initial state of the backend.
        -> Display          -- ^ Display config.
	-> Color            -- ^ Background color.
	-> Picture          -- ^ The picture to draw.
	-> IO ()

displayWithBackend backend displayMode background picture
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

		-- Escape exits the program
		, callback_exit () 
		
		-- Viewport control with mouse
		, callback_viewPort_keyMouse viewSR viewControlSR 
		, callback_viewPort_motion   viewSR viewControlSR 
		, callback_viewPort_reshape ]

	createWindow backend displayMode background callbacks
