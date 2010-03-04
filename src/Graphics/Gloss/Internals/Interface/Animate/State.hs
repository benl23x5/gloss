
module Graphics.Gloss.Internals.Interface.Animate.State
	( State (..) 
	, stateInit )
where

-- | Animation State
data State
	= State
	{
	-- | Whether the animation is running.
	  stateAnimate			:: Bool

	-- | Whether this is the first frame of the animation.
	, stateAnimateStart		:: Bool

	-- | Number of msec the animation has been running for
	, stateAnimateTime		:: Int

	-- | The time when we entered the display callback for the current frame.
	, stateDisplayTime		:: Int			
	, stateDisplayTimeLast		:: Int

	-- | Clamp the minimum time between frames to this value (in msec)
	--	Most LCD monitors refresh at around 50Hz so setting
	--	this to < 20msec probably isn't worthwhile.
	--
	, stateDisplayTimeClamp		:: Int			
							
	-- | The time when the last call to the users render function finished.
	, stateGateTimeStart		:: Int

	-- | The time when displayInWindow last finished (after sleeping to clamp fps).
	, stateGateTimeEnd		:: Int 
	
	-- | How long it took to draw this frame
	, stateGateTimeElapsed		:: Int }


stateInit
	= State
	{ stateAnimate			= True
	, stateAnimateStart		= True
	, stateAnimateTime		= 0
	, stateDisplayTime		= 0
	, stateDisplayTimeLast		= 0 
	, stateDisplayTimeClamp		= 20
	, stateGateTimeStart		= 0
	, stateGateTimeEnd		= 0 
	, stateGateTimeElapsed		= 0 }
