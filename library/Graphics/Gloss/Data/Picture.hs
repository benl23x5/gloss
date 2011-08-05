
-- | Data types for representing pictures.
module Graphics.Gloss.Data.Picture
	( Point
	, Vector
	, Path
	, Picture(..)

	-- * Aliases for Picture constructors
	, blank, polygon, line, circle, thickCircle, text, bitmap
	, color, translate, rotate, scale
	, pictures

	-- * Line loops
 	, lineLoop
	
	-- * Rectangles
	, rectanglePath, 	rectangleWire, 		rectangleSolid
	, rectangleUpperPath,	rectangleUpperWire, 	rectangleUpperSolid)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Data.Monoid
import Data.ByteString


-- | A path through the x-y plane.
type Path	= [Point]				

-- | A 2D picture
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

	-- | A bitmap image with a width, height and a ByteString holding the 32 bit RGBA bitmap data.
	| Bitmap	Int	Int 	ByteString

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


-- Instances -------------------------------------------------------------------------------------
instance Monoid Picture where
	mempty		= blank
	mappend a b	= Pictures [a, b]
	mconcat		= Pictures



-- Constructors ----------------------------------------------------------------------------------
blank :: Picture
blank	= Blank

polygon :: Path -> Picture
polygon = Polygon

line :: Path -> Picture
line 	= Line

circle :: Float -> Picture
circle 	= Circle

thickCircle :: Float -> Float -> Picture
thickCircle = ThickCircle

text :: String -> Picture
text = Text

bitmap :: Int -> Int -> ByteString -> Picture
bitmap = Bitmap

color :: Color -> Picture -> Picture
color = Color

translate :: Float -> Float -> Picture -> Picture
translate = Translate

rotate :: Float -> Picture -> Picture
rotate = Rotate

scale :: Float -> Float -> Picture -> Picture
scale = Scale

pictures :: [Picture] -> Picture
pictures = Pictures


-- Shapes ----------------------------------------------------------------------------------------
-- | A closed loop along this path.
lineLoop :: Path -> Picture
lineLoop []	= Line []
lineLoop (x:xs)	= Line ((x:xs) ++ [x])


-- | A path representing a rectangle centered about the origin,
--	with the given width and height.
rectanglePath :: Float -> Float -> Path
rectanglePath sizeX sizeY			
 = let	sx	= sizeX / 2
	sy	= sizeY / 2
   in	[(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle centered about the origin,
--	with the given width and height.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY
	= lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane,
--	with the given width and height.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY
	= lineLoop $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane,
--	with the given width and height
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy
 = let 	sx	= sizeX / 2
   in  	[(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin, 
--	with the given width and height.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY
	= Polygon $ rectanglePath sizeX sizeY


-- | A sold rectangle in the y > 0 half of the x-y plane,
--	with the given width and height.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY
	= Polygon  $ rectangleUpperPath sizeX sizeY

-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r = thickCircle (r/2) r

