{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Graphics.Gloss.Internals.Interface.ViewState.Motion
	(callback_viewState_motion)
where
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Internals.Interface.Callback
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Event
import Data.IORef


-- | Callback to handle keyboard and mouse button events
--	for controlling the viewport.
callback_viewState_motion 
	:: IORef ViewState
	-> Callback

callback_viewState_motion portRef
 	= Motion (viewState_motion portRef)


viewState_motion :: IORef ViewState -> MotionCallback
viewState_motion viewStateRef stateRef pos
 = do	viewState <- readIORef viewStateRef
	ev        <- motionEvent stateRef pos
        case updateViewStateWithEventMaybe ev viewState of
		Nothing -> return ()
		Just viewState' 
                 -> do	viewStateRef `writeIORef` viewState'
			postRedisplay stateRef


