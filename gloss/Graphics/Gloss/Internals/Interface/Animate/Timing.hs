{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Handles timing of animation.
--	The main point is that we want to restrict the framerate to something
--	sensible, instead of just displaying at the machines maximum possible
--	rate and soaking up 100% cpu. 
--
--	We also keep track of the elapsed time since the start of the program,
--	so we can pass this to the user's animation function.
-- 
module Graphics.Gloss.Internals.Interface.Animate.Timing
	( animateBegin
	, animateEnd )
where
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Animate.State
import Control.Monad
import Data.IORef


-- | Handles animation timing details.
--	Call this function at the start of each frame.
animateBegin :: IORef State -> DisplayCallback
animateBegin stateRef backendRef
 = do
	-- write the current time into the display state
	displayTime		<- elapsedTime backendRef
	displayTimeLast		<- stateRef `getsIORef` stateDisplayTime
	let displayTimeElapsed	= displayTime - displayTimeLast

	stateRef `modifyIORef` \s -> s 
		{ stateDisplayTime	= displayTime 
		, stateDisplayTimeLast	= displayTimeLast }

	-- increment the animation time
	animate         <- stateRef `getsIORef` stateAnimate
        animateCount    <- stateRef `getsIORef` stateAnimateCount
	animateTime	<- stateRef `getsIORef` stateAnimateTime
	animateStart	<- stateRef `getsIORef` stateAnimateStart

{-      when (animateCount `mod` 5 == 0)
         $  putStr  $  "  displayTime        = " ++ show displayTime                ++ "\n"
                    ++ "  displayTimeLast    = " ++ show displayTimeLast            ++ "\n"
                    ++ "  displayTimeElapsed = " ++ show displayTimeElapsed         ++ "\n"
                    ++ "  fps                = " ++ show (truncate $ 1 / displayTimeElapsed)   ++ "\n"
-}	
	when (animate && not animateStart)
	 $ stateRef `modifyIORef` \s -> s
	       { stateAnimateTime	= animateTime + displayTimeElapsed }
			
	when animate
	 $ stateRef `modifyIORef` \s -> s
	       { stateAnimateCount      = animateCount + 1
               , stateAnimateStart	= False  }



-- | Handles animation timing details.
--	Call this function at the end of each frame.
animateEnd :: IORef State -> DisplayCallback
animateEnd stateRef backendRef
 = do
	-- timing gate, limits the maximum frame frequency (FPS)
	timeClamp	<- stateRef `getsIORef` stateDisplayTimeClamp

	gateTimeStart	<- elapsedTime backendRef			-- the start of this gate
	gateTimeEnd	<- stateRef `getsIORef` stateGateTimeEnd	-- end of the previous gate
	let gateTimeElapsed = gateTimeStart - gateTimeEnd

	when (gateTimeElapsed < timeClamp)
	 $ do	sleep backendRef (timeClamp - gateTimeElapsed)

	gateTimeFinal	<- elapsedTime backendRef

	stateRef `modifyIORef` \s -> s 
		{ stateGateTimeEnd	= gateTimeFinal 
		, stateGateTimeElapsed	= gateTimeElapsed }


getsIORef :: IORef a -> (a -> r) -> IO r
getsIORef ref fun
 = liftM fun $ readIORef ref
