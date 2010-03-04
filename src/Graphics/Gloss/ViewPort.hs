
module Graphics.Gloss.ViewPort
	( ViewPort(..)
	, viewPortInit )
where
	
-- | Viewport state. 
--	These transformations are applied to the whole picture.
data ViewPort
	= ViewPort { 
	-- | The world is translated by this vector
	  viewPortTranslate	:: (Float, Float)

	-- | The world is rotated by this angle, in degrees.
	, viewPortRotate	:: Float		

	-- | The world is scaled by this factor in both x and y directions.
	, viewPortScale		:: Float		

	}
	
	
-- | The initial viewport state.
viewPortInit :: ViewPort
viewPortInit
	= ViewPort
	{ viewPortTranslate	= (0, 0) 
	, viewPortRotate	= 0
	, viewPortScale		= 1 
	}
