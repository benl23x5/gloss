
module Graphics.Gloss.Interface.ViewPort.Reshape
	(callback_viewPort_reshape)
where
import Graphics.Gloss.Interface.Callback
import Graphics.UI.GLUT					(($=), get)
import qualified Graphics.UI.GLUT			as GLUT
import qualified Graphics.Rendering.OpenGL.GL		as GL


-- | Callback to handle keyboard and mouse button events
--	for controlling the viewport.
callback_viewPort_reshape :: Callback

callback_viewPort_reshape
 	= Reshape (viewPort_reshape)

viewPort_reshape size
 = do
	-- Setup the viewport
	--	This controls what part of the window openGL renders to.
	--	We'll use the whole window.
	--
 	GL.viewport 	$= (GL.Position 0 0, size)
	GLUT.postRedisplay Nothing
