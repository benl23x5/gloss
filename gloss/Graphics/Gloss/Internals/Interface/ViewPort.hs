
-- | The 'ViewPort' represents the global transformation applied to the displayed picture.
--	When the user pans, zooms, or rotates the display then this changes the 'ViewPort'.
module Graphics.Gloss.Internals.Interface.ViewPort
	( ViewPort(..)
	, viewPortInit 
	, applyViewPortToPicture )
where
import Graphics.Gloss.Data.Picture (Picture(..))

data ViewPort
	= ViewPort { 
	-- | Global translation.
	  viewPortTranslate	:: !(Float, Float)

	-- | Global rotation (in degrees).
	, viewPortRotate	:: !Float		

	-- | Global scaling (of both x and y coordinates).
	, viewPortScale		:: !Float		
	}
	
	
-- | The initial state of the viewport.
viewPortInit :: ViewPort
viewPortInit
	= ViewPort
	{ viewPortTranslate	= (0, 0) 
	, viewPortRotate	= 0
	, viewPortScale		= 1 
	}

applyViewPortToPicture :: ViewPort  -> Picture -> Picture
applyViewPortToPicture port
	= Translate transX transY . Rotate rotate . Scale scale scale
	where	rotate	= realToFrac $ viewPortRotate port
        	transX	= realToFrac $ fst $ viewPortTranslate port
        	transY	= realToFrac $ snd $ viewPortTranslate port
        	scale	= realToFrac $ viewPortScale port
