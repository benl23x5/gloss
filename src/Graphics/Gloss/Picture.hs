
-- | Data types for representing pictures.
module Graphics.Gloss.Picture
	( Point
	, Vector
	, Path
	, Picture(..))
where

import Graphics.Gloss.Color

-- | A point on the x-y plane.
type Point	= (Float, Float)			

-- | A vector can be treated as a point, and vis-versa.
type Vector	= Point

-- | A path through the x-y plane.
type Path	= [Point]				

-- | A 2D picture.
data Picture
	-- Primitives -------------------------------------

	-- | A blank picture, with nothing in it.
	= Blank

	-- | A polygon filled with a solid color.
	| Polygon 	Path
	
	-- | A line along an arbitrary path.
	| Line		Path

	-- | A circle with the given radius.
	| Circle	Float

	-- | A circle with the given thickness and radius. If the thickness is 0 then this is equivalent to `Circle`.
	| ThickCircle	Float		Float

	-- | Some text to draw with a vector font.
	| Text		String

	-- Color ------------------------------------------
	-- | A picture drawn with this color.
	| Color		Color  		Picture

	-- Transforms -------------------------------------
	-- | A picture translated by the given x and y coordinates.
	| Translate	Float Float	Picture

	-- | A picture rotated by the given angle (in degrees).
	| Rotate	Float		Picture

	-- | A picture scaled by the given x and y factors.
	| Scale		Float	Float	Picture

	-- More Pictures ----------------------------------

	-- | A picture consisting of several others.
	| Pictures	[Picture]
	deriving (Show, Eq)

