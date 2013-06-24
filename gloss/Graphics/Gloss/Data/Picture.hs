
-- | Data types for representing pictures.
module Graphics.Gloss.Data.Picture
	( Point
	, Vector
	, Path
	, Picture(..)
	, BitmapData

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
        , rectangleUpperSolid

        -- * Loading Bitmaps
        , bitmapOfForeignPtr
        , bitmapOfByteString
        , bitmapOfBMP
        , loadBMP)

where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Internals.Render.Bitmap
import Codec.BMP
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.Word
import Data.Monoid
import Data.ByteString
import Data.Data
import System.IO.Unsafe
import qualified Data.ByteString.Unsafe as BSU
import Prelude hiding (map)

-- | A path through the x-y plane.
type Path	= [Point]				


-- | A 2D picture
data Picture
	-- Primitives -------------------------------------

	-- | A blank picture, with nothing in it.
	= Blank

	-- | A convex polygon filled with a solid color.
	| Polygon 	Path
	
	-- | A line along an arbitrary path.
	| Line		Path

	-- | A circle with the given radius.
	| Circle	Float

	-- | A circle with the given thickness and radius.
	--   If the thickness is 0 then this is equivalent to `Circle`.
	| ThickCircle	Float Float

	-- | A circular arc drawn counter-clockwise between two angles 
        --  (in degrees) at the given radius.
        | Arc           Float Float Float

	-- | A circular arc drawn counter-clockwise between two angles 
        --  (in degrees), with the given radius  and thickness.
	--   If the thickness is 0 then this is equivalent to `Arc`.
        | ThickArc	Float Float Float Float

	-- | Some text to draw with a vector font.
	| Text		String

	-- | A bitmap image with a width, height and some 32-bit RGBA
        --   bitmap data.
	-- 
	--  The boolean flag controls whether Gloss should cache the data
        --  between frames for speed. If you are programatically generating
        --  the image for each frame then use `False`. If you have loaded it
        --  from a file then use `True`.
	| Bitmap	Int	Int 	BitmapData Bool

	-- Color ------------------------------------------
	-- | A picture drawn with this color.
	| Color		Color  		Picture

	-- Transforms -------------------------------------
	-- | A picture translated by the given x and y coordinates.
	| Translate	Float Float	Picture

	-- | A picture rotated clockwise by the given angle (in degrees).
	| Rotate	Float		Picture

	-- | A picture scaled by the given x and y factors.
	| Scale		Float	Float	Picture

	-- More Pictures ----------------------------------
	-- | A picture consisting of several others.
	| Pictures	[Picture]
	deriving (Show, Eq, Data, Typeable)


-- Instances ------------------------------------------------------------------
instance Monoid Picture where
	mempty		= blank
	mappend a b	= Pictures [a, b]
	mconcat		= Pictures


-- Constructors ----------------------------------------------------------------
-- NOTE: The docs here should be identical to the ones on the constructors.

-- | A blank picture, with nothing in it.
blank :: Picture
blank	= Blank

-- | A convex polygon filled with a solid color.
polygon :: Path -> Picture
polygon = Polygon

-- | A line along an arbitrary path.
line :: Path -> Picture
line 	= Line

-- | A circle with the given radius.
circle  :: Float  -> Picture
circle 	= Circle

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


-- Bitmaps --------------------------------------------------------------------
-- | O(1). Use a `ForeignPtr` of RGBA data as a bitmap with the given
--   width and height.

--   The boolean flag controls whether Gloss should cache the data
--   between frames for speed. If you are programatically generating
--   the image for each frame then use `False`. If you have loaded it
--   from a file then use `True`.
bitmapOfForeignPtr :: Int -> Int -> ForeignPtr Word8 -> Bool -> Picture
bitmapOfForeignPtr width height fptr cacheMe
 = let  len     = width * height * 4
        bdata   = BitmapData len fptr
   in   Bitmap width height bdata cacheMe 


-- | O(size). Copy a `ByteString` of RGBA data into a bitmap with the given
--   width and height.
--
--   The boolean flag controls whether Gloss should cache the data
--   between frames for speed. If you are programatically generating
--   the image for each frame then use `False`. If you have loaded it
--   from a file then use `True`.
{-# NOINLINE bitmapOfByteString #-}
bitmapOfByteString :: Int -> Int -> ByteString -> Bool -> Picture
bitmapOfByteString width height bs cacheMe
 = unsafePerformIO
 $ do   let len = width * height * 4
        ptr     <- mallocBytes len
        fptr    <- newForeignPtr finalizerFree ptr

        BSU.unsafeUseAsCString bs
         $ \cstr -> copyBytes ptr (castPtr cstr) len

        let bdata = BitmapData len fptr
        return $ Bitmap width height bdata cacheMe


-- | O(size). Copy a `BMP` file into a bitmap.
{-# NOINLINE bitmapOfBMP #-}
bitmapOfBMP :: BMP -> Picture
bitmapOfBMP bmp
 = unsafePerformIO
 $ do   let (width, height)     = bmpDimensions bmp
        let bs                  = unpackBMPToRGBA32 bmp 
        let len                 = width * height * 4

        ptr     <- mallocBytes len
        fptr    <- newForeignPtr finalizerFree ptr

        BSU.unsafeUseAsCString bs
         $ \cstr -> copyBytes ptr (castPtr cstr) len

        let bdata = BitmapData len fptr
        reverseRGBA bdata

        return $ Bitmap width height bdata True


-- | Load an uncompressed 24 or 32bit RGBA BMP file as a bitmap.
loadBMP :: FilePath -> IO Picture
loadBMP filePath
 = do   ebmp    <- readBMP filePath
        case ebmp of
         Left err       -> error $ show err
         Right bmp      -> return $ bitmapOfBMP bmp


-- Other Shapes ---------------------------------------------------------------
-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop []	= Line []
lineLoop (x:xs)	= Line ((x:xs) ++ [x])


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
 = let	sx	= sizeX / 2
	sy	= sizeY / 2
   in	[(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


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
 = let 	sx	= sizeX / 2
   in  	[(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY
	= Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY
	= Polygon  $ rectangleUpperPath sizeX sizeY

