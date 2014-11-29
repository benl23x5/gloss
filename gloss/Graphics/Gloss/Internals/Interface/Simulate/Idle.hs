{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Internals.Interface.Simulate.Idle
	( callback_simulate_idle )
where
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Internals.Interface.Callback
import qualified Graphics.Gloss.Internals.Interface.Backend		as Backend
import qualified Graphics.Gloss.Internals.Interface.Animate.State	as AN
import qualified Graphics.Gloss.Internals.Interface.Simulate.State	as SM
import Data.IORef
import Control.Monad
import GHC.Float (double2Float)


-- | The graphics library calls back on this function when it's finished drawing
--	and it's time to do some computation.
callback_simulate_idle
	:: IORef SM.State				-- ^ the simulation state
	-> IORef AN.State				-- ^ the animation statea
	-> IO ViewPort
        -- ^ action to get the 'ViewPort'.  We don't use an 'IORef'
        -- directly because sometimes we hold a ref to a 'ViewPort' (in
        -- Game) and sometimes a ref to a 'ViewState'.
	-> IORef world					-- ^ the current world
	-> (ViewPort -> Float -> world -> IO world) 	-- ^ fn to advance the world
	-> Float					-- ^ how much time to advance world by 
							--	in single step mode
	-> IdleCallback
	
callback_simulate_idle simSR animateSR viewSA worldSR worldAdvance _singleStepTime backendRef
 = {-# SCC "callbackIdle" #-}
   do	simulate_run simSR animateSR viewSA worldSR worldAdvance backendRef
 

-- take the number of steps specified by controlWarp
simulate_run 
	:: IORef SM.State
	-> IORef AN.State
	-> IO ViewPort
	-> IORef world
	-> (ViewPort -> Float -> world -> IO world)
	-> IdleCallback
	
simulate_run simSR _ viewSA worldSR worldAdvance backendRef
 = do	viewS		<- viewSA
	simS		<- readIORef simSR
	worldS		<- readIORef worldSR

	-- get the elapsed time since the start simulation (wall clock)
 	elapsedTime	<- fmap double2Float $ Backend.elapsedTime backendRef

	-- get how far along the simulation is
	simTime			<- simSR `getsIORef` SM.stateSimTime
 
 	-- we want to simulate this much extra time to bring the simulation
	--	up to the wall clock.
	let thisTime	= elapsedTime - simTime
	 
	-- work out how many steps of simulation this equals
	resolution	<- simSR `getsIORef` SM.stateResolution
	let timePerStep	= 1 / fromIntegral resolution
	let thisSteps_	= truncate $ fromIntegral resolution * thisTime
	let thisSteps	= if thisSteps_ < 0 then 0 else thisSteps_

	let newSimTime	= simTime + fromIntegral thisSteps * timePerStep
	 
{-	putStr	$  "elapsed time    = " ++ show elapsedTime 	++ "\n"
		++ "sim time        = " ++ show simTime		++ "\n"
		++ "this time       = " ++ show thisTime	++ "\n"
		++ "this steps      = " ++ show thisSteps	++ "\n"
		++ "new sim time    = " ++ show newSimTime	++ "\n"
		++ "taking          = " ++ show thisSteps	++ "\n\n"
-}
 	-- work out the final step number for this display cycle
	let nStart	= SM.stateIteration simS
	let nFinal 	= nStart + thisSteps

	-- keep advancing the world until we get to the final iteration number
	(_,world') <- untilM 	(\(n, _) 	-> n >= nFinal)
				(\(n, w)	-> liftM (\w' -> (n+1,w')) ( worldAdvance viewS timePerStep w))
				(nStart, worldS)

	-- write the world back into its IORef
	-- We need to seq on the world to avoid space leaks when the window is not showing.
	world' `seq` writeIORef worldSR world'

	-- update the control state
	simSR `modifyIORef` \c -> c
		{ SM.stateIteration	= nFinal
		, SM.stateSimTime	= newSimTime }
	
	-- tell glut we want to draw the window after returning
	Backend.postRedisplay backendRef


getsIORef :: IORef a -> (a -> r) -> IO r
getsIORef ref fun
 = liftM fun $ readIORef ref

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM test op i = go i
  where
  go x | test x    = return x
       | otherwise = op x >>= go
	
