
module Graphics.Gloss.Internals.Interface.Display
	(displayWithBackend)
where	
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Rendering
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewState.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewState.Motion
import Graphics.Gloss.Internals.Interface.ViewState.Reshape
import qualified Graphics.Gloss.Internals.Interface.Callback as Callback
import Data.IORef
import Data.Functor
import System.Mem


displayWithBackend
	:: Backend a
	=> a                -- ^ Initial state of the backend.
        -> Display          -- ^ Display config.
	-> Color            -- ^ Background color.
	-> Picture          -- ^ The picture to draw.
	-> IO ()

displayWithBackend backend displayMode background picture
 =  do	viewSR		<- newIORef viewStateInit

        renderS         <- initState
	renderSR	<- newIORef renderS
	
	let renderFun backendRef = do
		port      <- viewStateViewPort <$> readIORef viewSR
		options	  <- readIORef renderSR
                windowSize <- getWindowDimensions backendRef

                displayPicture 
                        windowSize
                        background
                        options
                        (viewPortScale port)
                        (applyViewPortToPicture port picture)

                -- perform GC every frame to try and avoid long pauses
                performGC

	let callbacks
	     =	[ Callback.Display renderFun 

		-- Escape exits the program
		, callback_exit () 
		
		-- Viewport control with mouse
		, callback_viewState_keyMouse viewSR
		, callback_viewState_motion   viewSR
		, callback_viewState_reshape ]

	createWindow backend displayMode background callbacks
