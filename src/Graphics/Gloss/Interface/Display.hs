
module Graphics.Gloss.Interface.Display
	(displayInWindow)
where	

import Graphics.Gloss.ViewPort
import Graphics.Gloss.Picture
import Graphics.Gloss.Render.Picture
import Graphics.Gloss.Render.ViewPort
import Graphics.Gloss.Interface.Window
import qualified Graphics.Gloss.Render.Options		as RO

import Graphics.Gloss.Interface.Callback.Exit
import qualified Graphics.Gloss.Interface.Callback	as Callback

import Data.IORef

-- | Create a new window and display the given picture.
displayInWindow
	:: String	-- ^ Name of the window.
	-> (Int, Int)	-- ^ Initial size of the window, in pixels.
	-> (Int, Int)	-- ^ Initial position of the window, in pixels.
	-> Picture	-- ^ The picture to draw.
	-> IO ()

displayInWindow name size pos picture
 =  do
	viewSR		<- newIORef viewPortInit
	renderSR	<- newIORef RO.optionsInit
	
	let renderFun = do
		view	<- readIORef viewSR
		options	<- readIORef renderSR
	 	withViewPort
	 		view
			(renderPicture options view picture)

	let callbacks
	     =	[ Callback.Display renderFun 
		, callback_exit () ]

	createWindow name size pos callbacks
