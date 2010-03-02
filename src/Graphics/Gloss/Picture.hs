
-- | Picture data types.
module Graphics.Gloss.Picture
	( Point
	, Vector
	, Path
	, Picture(..))
where

import Graphics.Gloss.Color

-- | A point on the x-y plane.
type Point	= (Float, Float)			

-- | A vector can be treated as a point, and visa-versa
type Vector	= Point

-- | A path through the x-y plane.
type Path	= [Point]				

-- | Basic picture components.
data Picture
	-- Primitives -------------------------------------

	-- | A blank picture, with nothing in it.
	= Blank
	
	-- | A line along an arbitrary path.
	| Line		Path

	-- |  A polygon filled with a solid color.
	| Polygon 	Path

	-- | A circle with the given radius and thickness.
	| Circle	Float	Float

	-- | Some text to draw with a vector font.
	| Text		String

	-- Color ------------------------------------------
	-- |  A picture drawn with this color.
	| Color		Color  		Picture

	-- Transforms -------------------------------------
	-- | Translate a picture
	| Translate	Float Float	Picture

	-- | Rotate a picture, in degrees
	| Rotate	Float		Picture

	-- | Scale a picture
	| Scale		Float	Float	Picture

	-- More Pictures ----------------------------------
	| Pictures	[Picture]
	deriving (Show, Eq)

