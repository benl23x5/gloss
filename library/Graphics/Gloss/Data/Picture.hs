
-- | Data types for representing pictures.
module Graphics.Gloss.Data.Picture
	( Point
	, Vector
	, Path
	, Picture(..)
	, BitmapData

	-- * Aliases for Picture constructors
	, blank, polygon, line, circle, thickCircle, text, bitmap
	, color, translate, rotate, scale
	, pictures

        -- * Loading Bitmaps
        , bitmapOfForeignPtr
	, bitmapOfByteString
	, bitmapOfBMP
	, loadBMP

	-- * Miscellaneous
 	, lineLoop
 	, circleSolid
	
	-- * Rectangles
	, rectanglePath, 	rectangleWire, 		rectangleSolid
	, rectangleUpperPath,	rectangleUpperWire, 	rectangleUpperSolid)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Internals.Render.Bitmap
import Control.Monad
import Codec.BMP
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.Word
import Data.Monoid
import Data.ByteString
import System.IO.Unsafe
import qualified Data.ByteString.Unsafe as BSU


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
	| ThickCircle	Float		Float

	-- | Some text to draw with a vector font.
	| Text		String

	-- | A bitmap image with a width, height and a Vector holding the 32 bit RGBA bitmap data.
	-- 
	--  The boolean flag controls whether Gloss should cache the data between frames
	--  for speed. If you are programatically generating the image for each frame then use
	--  `False`.  If you have loaded it from a file then use `True`.
	| Bitmap	Int	Int 	BitmapData Bool

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

bitmap :: Int -> Int -> BitmapData -> Bool -> Picture
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


-- Bitmaps --------------------------------------------------------------------
-- | O(1). Use a `ForeignPtr` of RGBA data as a bitmap.
bitmapOfForeignPtr :: Int -> Int -> ForeignPtr Word8 -> Bool -> Picture
bitmapOfForeignPtr width height fptr cacheMe
 = let  len     = width * height * 4
        bdata   = BitmapData len fptr
   in   Bitmap width height bdata cacheMe 


-- | O(size). Copy a `ByteString` of RGBA data into a bitmap.
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


-- | A solid rectangle in the y > 0 half of the x-y plane,
--	with the given width and height.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY
	= Polygon  $ rectangleUpperPath sizeX sizeY

-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r = thickCircle (r/2) r

