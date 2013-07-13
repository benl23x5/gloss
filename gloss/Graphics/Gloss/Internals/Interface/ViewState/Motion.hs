{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Graphics.Gloss.Internals.Interface.ViewState.Motion
	(callback_viewState_motion)
where
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Interface.Callback
import Graphics.Gloss.Internals.Interface.Backend
import Data.IORef


-- | Callback to handle keyboard and mouse button events
--	for controlling the viewport.
callback_viewState_motion 
	:: IORef ViewState
	-> Callback

callback_viewState_motion portRef
 	= Motion (viewState_motion portRef)

viewState_motion
	:: IORef ViewState
	-> MotionCallback

viewState_motion viewStateRef stateRef (posX, posY)
 = do	viewState <- readIORef viewStateRef
	let	ev = EventMotion (fromIntegral posX, fromIntegral posY)
        case updateViewStateWithEvent' ev viewState of
		Nothing -> return ()
		Just viewState' ->
		  do	viewStateRef `writeIORef` viewState'
			postRedisplay stateRef


