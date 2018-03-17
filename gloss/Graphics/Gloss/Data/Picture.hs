
module Graphics.Gloss.Data.Picture
        ( Picture       (..)
        , Point, Vector, Path

        -- * Aliases for Picture constructors
        , blank
        , polygon
        , line
        , circle, thickCircle
        , arc,    thickArc
        , text
        , bitmap
        , color
        , translate, rotate, scale
        , pictures

        -- * Compound shapes
        , lineLoop
        , circleSolid
        , arcSolid
        , sectorWire
        , rectanglePath
        , rectangleWire
        , rectangleSolid
        , rectangleUpperPath
        , rectangleUpperWire
        , rectangleUpperSolid)
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Angle


-- Constructors ----------------------------------------------------------------
-- NOTE: The docs here should be identical to the ones on the constructors.

-- | A blank picture, with nothing in it.
blank :: Picture
blank   = Blank

-- | A convex polygon filled with a solid color.
polygon :: Path -> Picture
polygon = Polygon

-- | A line along an arbitrary path.
line :: Path -> Picture
line    = Line

-- | A circle with the given radius.
circle  :: Float  -> Picture
circle  = Circle

-- | A circle with the given thickness and radius.
--   If the thickness is 0 then this is equivalent to `Circle`.
thickCircle  :: Float -> Float -> Picture
thickCircle = ThickCircle

-- | A circular arc drawn counter-clockwise between two angles (in degrees)
--   at the given radius.
arc     :: Float -> Float -> Float -> Picture
arc = Arc

-- | A circular arc drawn counter-clockwise between two angles (in degrees),
--   with the given radius  and thickness.
--   If the thickness is 0 then this is equivalent to `Arc`.
thickArc :: Float -> Float -> Float -> Float -> Picture
thickArc = ThickArc

-- | Some text to draw with a vector font.
text :: String -> Picture
text = Text

-- | A bitmap image with a width, height and a Vector holding the
--   32-bit RGBA bitmap data.
--
--  The boolean flag controls whether Gloss should cache the data
--  between frames for speed.
--  If you are programatically generating the image for
--  each frame then use `False`.
--  If you have loaded it from a file then use `True`.
bitmap  :: Int -> Int -> BitmapData -> Bool -> Picture
bitmap = Bitmap

-- | A picture drawn with this color.
color :: Color -> Picture -> Picture
color = Color

-- | A picture translated by the given x and y coordinates.
translate :: Float -> Float -> Picture -> Picture
translate = Translate

-- | A picture rotated clockwise by the given angle (in degrees).
rotate  :: Float -> Picture -> Picture
rotate = Rotate

-- | A picture scaled by the given x and y factors.
scale   :: Float -> Float -> Picture -> Picture
scale = Scale

-- | A picture consisting of several others.
pictures :: [Picture] -> Picture
pictures = Pictures


-- Other Shapes ---------------------------------------------------------------
-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop []     = Line []
lineLoop (x:xs) = Line ((x:xs) ++ [x])


-- Circles and Arcs -----------------------------------------------------------
-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r
        = thickCircle (r/2) r


-- | A solid arc, drawn counter-clockwise between two angles at the given radius.
arcSolid  :: Float -> Float -> Float -> Picture
arcSolid a1 a2 r
        = thickArc a1 a2 (r/2) r


-- | A wireframe sector of a circle.
--   An arc is draw counter-clockwise from the first to the second angle at
--   the given radius. Lines are drawn from the origin to the ends of the arc.
---
--   NOTE: We take the absolute value of the radius incase it's negative.
--   It would also make sense to draw the sector flipped around the
--   origin, but I think taking the absolute value will be less surprising
--   for the user.
--
sectorWire :: Float -> Float -> Float -> Picture
sectorWire a1 a2 r_
 = let r        = abs r_
   in  Pictures
        [ Arc a1 a2 r
        , Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))] ]


-- Rectangles -----------------------------------------------------------------
-- NOTE: Only the first of these rectangle functions has haddocks on the
--       arguments to reduce the amount of noise in the extracted docs.

-- | A path representing a rectangle centered about the origin
rectanglePath
        :: Float        -- ^ width of rectangle
        -> Float        -- ^ height of rectangle
        -> Path
rectanglePath sizeX sizeY
 = let  sx      = sizeX / 2
        sy      = sizeY / 2
   in   [(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle centered about the origin.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY
        = lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY
        = lineLoop $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy
 = let  sx      = sizeX / 2
   in   [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY
        = Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY
        = Polygon  $ rectangleUpperPath sizeX sizeY

