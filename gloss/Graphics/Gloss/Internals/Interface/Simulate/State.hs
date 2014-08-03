{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.Simulate.State
	( State (..)
	, stateInit )
where

-- | Simulation state
data State	
 = 	State
	{ -- | The iteration number we're up to.
	  stateIteration	:: !Integer

	-- | How many simulation setps to take for each second of real time
	, stateResolution	:: !Int 
	
	-- | How many seconds worth of simulation we've done so far
	, stateSimTime		:: !Float
	
	-- | Record how many steps we've been taking per frame
	, stateStepsPerFrame 	:: !Int  }
	

-- | Initial control state
stateInit :: Int -> State
stateInit resolution
 	= State
 	{ stateIteration		= 0
	, stateResolution		= resolution 
	, stateSimTime			= 0 
	, stateStepsPerFrame		= 0 }
	
	
