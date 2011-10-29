{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.ViewPort.Reshape
	(callback_viewPort_reshape)
where
import Graphics.Gloss.Internals.Interface.Callback
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Rendering.OpenGL			(($=))
import qualified Graphics.Rendering.OpenGL.GL		as GL


-- | Callback to handle keyboard and mouse button events
--	for controlling the viewport.
callback_viewPort_reshape :: Callback
callback_viewPort_reshape
 	= Reshape (viewPort_reshape)


viewPort_reshape :: ReshapeCallback
viewPort_reshape stateRef (width,height)
 = do
	-- Setup the viewport
	--	This controls what part of the window openGL renders to.
	--	We'll use the whole window.
	--
 	GL.viewport 	$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
	postRedisplay stateRef
