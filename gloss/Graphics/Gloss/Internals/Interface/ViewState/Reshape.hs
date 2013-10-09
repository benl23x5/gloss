{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.ViewState.Reshape
	(callback_viewState_reshape, viewState_reshape)
where
import Graphics.Gloss.Internals.Interface.Callback
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Rendering.OpenGL			(($=))
import qualified Graphics.Rendering.OpenGL.GL		as GL


-- | Callback to handle keyboard and mouse button events
--	for controlling the viewport.
callback_viewState_reshape :: Callback
callback_viewState_reshape
 	= Reshape (viewState_reshape)


viewState_reshape :: ReshapeCallback
viewState_reshape stateRef (width,height)
 = do
	-- Setup the viewport
	--	This controls what part of the window openGL renders to.
	--	We'll use the whole window.
	--
 	GL.viewport 	$= ( GL.Position 0 0
                           , GL.Size (fromIntegral width) (fromIntegral height))
	postRedisplay stateRef
