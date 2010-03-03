
-- | Handles timing of animation.
--	The main point is that we want to restrict the framerate to something
--	sensible, instead of just displaying at the machines maximum possible
--	rate and soaking up 100% cpu. 
--
--	We also keep track of the elapsed time since the start of the program,
--	so we can pass this to the user's animation function.
-- 
module Graphics.Gloss.Interface.Animate.Timing
	( animateBegin
	, animateEnd )
where
import Graphics.Gloss.Interface.Animate.State
import Control.Monad
import Control.Concurrent
import Data.IORef
import qualified Graphics.UI.GLUT			as GLUT
import qualified Graphics.Rendering.OpenGL.GL		as GL
import Graphics.UI.GLUT					(($=), get)


-- | Handles animation timing details.
--	Call this function at the start of each frame.
animateBegin :: IORef State -> IO ()
animateBegin stateRef
 = do
	-- write the current time into the display state
	displayTime		<- get GLUT.elapsedTime
	displayTimeLast		<- stateRef `getsIORef` stateDisplayTime
	let displayTimeElapsed	= displayTime - displayTimeLast

	stateRef `modifyIORef` \s -> s 
		{ stateDisplayTime	= displayTime 
		, stateDisplayTimeLast	= displayTimeLast }

{-	putStr 	$  "  displayTime        = " ++ show displayTime 		++ "\n"
	 	++ "  displayTimeLast    = " ++ show displayTimeLast 		++ "\n"
		++ "  displayTimeElapsed = " ++ show displayTimeElapsed 	++ "\n"
-}

	-- increment the animation time
	animate			<- stateRef `getsIORef` stateAnimate
	animateTime		<- stateRef `getsIORef` stateAnimateTime
	animateStart		<- stateRef `getsIORef` stateAnimateStart
	
	when (animate && not animateStart)
	 $ do	stateRef `modifyIORef` \s -> s
			{ stateAnimateTime	= animateTime + displayTimeElapsed }
			
	when animate
	 $ do	stateRef `modifyIORef` \s -> s
	 		{ stateAnimateStart	= False }


-- | Handles animation timing details.
--	Call this function at the end of each frame.
animateEnd :: IORef State -> IO ()
animateEnd stateRef
 = do
	-- timing gate, limits the maximum frame frequency (FPS)
	timeClamp	<- stateRef `getsIORef` stateDisplayTimeClamp

	gateTimeStart	<- get GLUT.elapsedTime				-- the start of this gate
	gateTimeEnd	<- stateRef `getsIORef` stateGateTimeEnd	-- end of the previous gate
	let gateTimeElapsed	
			= gateTimeStart - gateTimeEnd
	
	when (gateTimeElapsed < timeClamp)
	 $ do	threadDelay ((timeClamp - gateTimeElapsed) * 1000)

	gateTimeFinal	<- get GLUT.elapsedTime

	stateRef `modifyIORef` \s -> s 
		{ stateGateTimeEnd	= gateTimeFinal 
		, stateGateTimeElapsed	= gateTimeElapsed }


getsIORef ref fun
 = liftM fun $ readIORef ref
