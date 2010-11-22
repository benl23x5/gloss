
-- | The 'ViewPort' represents the global transformation applied to the displayed picture.
--	When the user pans, zooms, or rotates the display then this changes the 'ViewPort'.
module Graphics.Gloss.Internals.Interface.ViewPort
	( ViewPort(..)
	, viewPortInit )
where
	
data ViewPort
	= ViewPort { 
	-- | Global translation.
	  viewPortTranslate	:: (Float, Float)

	-- | Global rotation (in degrees).
	, viewPortRotate	:: Float		

	-- | Global scaling (of both x and y coordinates).
	, viewPortScale		:: Float		
	}
	
	
-- | The initial state of the viewport.
viewPortInit :: ViewPort
viewPortInit
	= ViewPort
	{ viewPortTranslate	= (0, 0) 
	, viewPortRotate	= 0
	, viewPortScale		= 1 
	}
