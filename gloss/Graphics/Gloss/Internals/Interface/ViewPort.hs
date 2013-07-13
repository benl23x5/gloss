
-- | The 'ViewPort' represents the global transformation applied to the displayed picture.
--	When the user pans, zooms, or rotates the display then this changes the 'ViewPort'.
module Graphics.Gloss.Internals.Interface.ViewPort
	( ViewPort(..)
	, viewPortInit
	, applyViewPortToPicture
	, invertViewPort )
where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Picture (Picture(..))
import Graphics.Gloss.Data.Point

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

-- | Translates, rotates, and scales an image according to the 'ViewPort'.
applyViewPortToPicture :: ViewPort  -> Picture -> Picture
applyViewPortToPicture port
	= Scale scale scale . Rotate rotate . Translate transX transY
	where	rotate	= realToFrac $ viewPortRotate port
        	transX	= realToFrac $ fst $ viewPortTranslate port
        	transY	= realToFrac $ snd $ viewPortTranslate port
        	scale	= realToFrac $ viewPortScale port

-- | Takes a point referencing a position according to the 'ViewPort'
--   (e.g. user input) and reverts the scaling, rotation, and
--   translation.
invertViewPort :: ViewPort -> Point -> Point
invertViewPort
	ViewPort	{ viewPortScale		= scale
			, viewPortTranslate	= trans
			, viewPortRotate	= r }
        pos
	= rotateV (degToRad (r + 360)) (mulSV (1 / scale) pos) - trans
