
module Graphics.Gloss.Interface.ViewPort.State
	( State(..)
	, stateInit )
where
	
-- | Viewport state. 
--	These transformations are applied to the whole picture.
data State
	= State

	-- | The world is scaled by this factor in both x and y directions.
	{ stateScale		:: Float		
								
	-- | The world is translated by this vector. 
	, stateTranslate	:: (Float, Float)

	-- | The world is rotated by this angle, in degrees.
	, stateRotate		:: Float		
	}
	
	
-- | The initial view state.
stateInit :: State
stateInit
	= State
	{ stateScale		= 1 
	, stateTranslate	= (0, 0) 
	, stateRotate		= 0
	}
