{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternGuards, RankNTypes #-}

module Graphics.Gloss.Internals.Interface.ViewState.KeyMouse
	(callback_viewState_keyMouse)
where
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Interface.Backend
import Data.IORef


-- | Callback to handle keyboard and mouse button events
--	for controlling the 'ViewState'.
callback_viewState_keyMouse 
	:: IORef ViewState
	-> Callback

callback_viewState_keyMouse viewStateRef
 	= KeyMouse (viewState_keyMouse viewStateRef)


-- TODO maybe avoid redisplaying if not needed
viewState_keyMouse
	:: IORef ViewState
	-> KeyboardMouseCallback
viewState_keyMouse viewStateRef stateRef key keyState keyMods (posX, posY)
 = do	viewStateRef `modifyIORef`
		updateViewStateWithEvent (EventKey key keyState keyMods (fromIntegral posX, fromIntegral posY))
	postRedisplay stateRef
