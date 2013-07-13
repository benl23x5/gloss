
module Graphics.Gloss.Internals.Interface.Display
	(displayWithBackend)
where	
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Render.Common
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewState.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewState.Motion
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import qualified Graphics.Gloss.Internals.Render.State	        		as RS
import qualified Graphics.Gloss.Internals.Interface.Callback			as Callback

import Data.IORef
import Data.Functor


displayWithBackend
	:: Backend a
	=> a                -- ^ Initial state of the backend.
        -> Display          -- ^ Display config.
	-> Color            -- ^ Background color.
	-> Picture          -- ^ The picture to draw.
	-> IO ()

displayWithBackend backend displayMode background picture
 =  do	viewSR		<- newIORef viewStateInit

        renderS         <- RS.stateInit
	renderSR	<- newIORef renderS
	
	let renderFun backendRef = do
		port    <- viewStateViewPort <$> readIORef viewSR
		options	<- readIORef renderSR
	 	renderAction
			backendRef
			(renderPicture backendRef options port picture)

	let callbacks
	     =	[ Callback.Display renderFun 

		-- Escape exits the program
		, callback_exit () 
		
		-- Viewport control with mouse
		, callback_viewState_keyMouse viewSR
		, callback_viewState_motion   viewSR
		, callback_viewState_reshape ]

	createWindow backend displayMode background callbacks
