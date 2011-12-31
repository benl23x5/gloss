
-- | Data types for representing pictures.
module Graphics.Gloss.Data.Picture
	( Point
	, Vector
	, Path
	, Picture(..)
	, BitmapData

	-- * Aliases for Picture constructors
	, blank, polygon, line, circle, thickCircle
        , arc, thickArc, sector
        , text, bitmap
	, color, translate, rotate, scale
	, pictures

        -- * Loading Bitmaps
        , bitmapOfForeignPtr
	, bitmapOfByteString
	, bitmapOfBMP
	, loadBMP

	-- * Miscellaneous
 	, lineLoop
 	, circleSolid, arcSolid, arcPath
	
	-- * Rectangles
	, rectanglePath, 	rectangleWire, 		rectangleSolid
	, rectangleUpperPath,	rectangleUpperWire, 	rectangleUpperSolid)
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
import System.IO.Unsafe
import qualified Data.ByteString.Unsafe as BSU
import Prelude hiding (map)
import qualified Prelude as P

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

	-- | A circular arc between two angles (a1 and a2) and
        --   with the given radius (r). The arc is drawn
        --   counter-clockwise from a1 to a2, where the angles  
        --   are in degrees.  
        | Arc	Float Float Float

	-- | A circular arc between two angles (a1 and a2) and
        --   with the given radius (r) and thickness.
	--   If the thickness is 0 then this is equivalent to `Arc`.
        | ThickArc	Float Float Float Float

	-- | A circular arc between two angles (a1 and a2) and
        --   with the given radius (r), with radial lines connecting 
        --   the ends to the center of the circle. The arc is drawn
        --   counter-clockwise from a1 to a2, where the angles  
        --   are in degrees. 
        | Sector	Float Float Float

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

	-- | A picture rotated clockwise by the given angle (in degrees).
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

-- | Create a `Blank` picture.
blank :: Picture
blank	= Blank

-- | Create a `Polygon`.
polygon :: Path -> Picture
polygon = Polygon

-- | Create a `Line`.
line :: Path -> Picture
line 	= Line

-- | Create a `Circle`.
circle ::
  Float       -- ^ radius
  -> Picture
circle 	= Circle

-- | Create a `ThickCircle`. Using a @thickness@ of 0
--   is the same as `circle`.
thickCircle :: 
  Float       -- ^ radius
  -> Float    -- ^ thickness
  -> Picture
thickCircle = ThickCircle

-- | Create an `Arc`, drawn counter-clockwise from the
--   start to end angles. The angles are measured 
--   counter-clockwise from the horizontal.
arc ::
  Float -- ^ start angle, in degrees
  -> Float -- ^ end angle, in degrees
  -> Float -- ^ radius
  -> Picture
arc = Arc

-- | Create a `ThickArc`. Using a @thickness@ of 0 is the
--   same as `arc`.
thickArc :: 
  Float -- ^ start angle, in degrees
  -> Float -- ^ end angle, in degrees
  -> Float -- ^ radius
  -> Float -- ^ thickness
  -> Picture
thickArc = ThickArc

-- | Draw a `Sector`, which is an `Arc` connected to the origin
--   of the circle.
sector :: 
  Float -- ^ start angle, in degrees
  -> Float -- ^ end angle, in degrees
  -> Float -- ^ radius
  -> Picture
sector = Sector

-- thickSector :: Float -> Float -> Float -> Float -> Picture
-- thickSector = ThickSector

-- | Add `Text` to the picture.
text :: String -> Picture
text = Text

-- | Add a `Bitmap`.
bitmap :: 
  Int            -- ^ width
  -> Int         -- ^ height
  -> BitmapData  -- ^ 32-bit RGBA bitmap data
  -> Bool        -- ^ should the data be cached between frames?
  -> Picture
bitmap = Bitmap

-- | Set the `Color` for a picture.
color :: Color -> Picture -> Picture
color = Color

-- | `Translate` a picture horizontally and vertically.
translate :: 
  Float       -- ^ x
  -> Float    -- ^ y
  -> Picture 
  -> Picture
translate = Translate

-- | `Rotate` a picture.
rotate :: 
  Float -- ^ angle, in degrees
  -> Picture -> Picture
rotate = Rotate

-- | `Scale` the size of a picture.
scale :: 
  Float       -- ^ x scale factor
  -> Float    -- ^ y scale factor
  -> Picture 
  -> Picture
scale = Scale

-- | Combine pictures.
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


-- | A path representing a rectangle centered about the origin.
rectanglePath :: 
  Float     -- ^ width. 
  -> Float  -- ^ height
  -> Path
rectanglePath sizeX sizeY			
 = let	sx	= sizeX / 2
	sy	= sizeY / 2
   in	[(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle centered about the origin.
rectangleWire :: 
  Float       -- ^ width
  -> Float    -- ^ height
  -> Picture
rectangleWire sizeX sizeY
	= lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: 
  Float       -- ^ width
  -> Float    -- ^ height
  -> Picture
rectangleUpperWire sizeX sizeY
	= lineLoop $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath ::
  Float       -- ^ width
  -> Float    -- ^ height
  -> Path
rectangleUpperPath sizeX sy
 = let 	sx	= sizeX / 2
   in  	[(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin.
rectangleSolid ::
  Float       -- ^ width
  -> Float    -- ^ height
  -> Picture
rectangleSolid sizeX sizeY
	= Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid ::
  Float       -- ^ width
  -> Float    -- ^ height
  -> Picture
rectangleUpperSolid sizeX sizeY
	= Polygon  $ rectangleUpperPath sizeX sizeY

-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r = thickCircle (r/2) r

-- | A solid arc, drawn counter-clockwise from the start
--   to the end angle.
arcSolid :: 
  Float      -- ^ Start angle, in degrees
  -> Float   -- ^ End angle, in degrees
  -> Float   -- ^ Radius of arc
  -> Picture
arcSolid a1 a2 r = thickArc a1 a2 (r/2) r 

-- Ideally we would hide the Int argument, using
-- Graphics.Gloss.Internals.Render.Circle.circleSteps, to
-- better match the behavior of circle, but it's not clear
-- how to do this.

-- | A path representing an arc, centered about the origin.
arcPath ::
  Float     -- ^ start angle, in degrees
  -> Float  -- ^ end angle, in degrees
  -> Float  -- ^ radius
  -> Int    -- ^ number of segments, assumed to be > 0
  -> Path
arcPath a1 a2 r n =  
  let  tStart    = degToRad a1
       tStop     = degToRad a2 + if a1 >= a2 then 2 * pi else 0
       tStep     = (tStop - tStart) / fromIntegral n
       
       -- not the most efficient
       arcPos t  = (r * cos t, r * sin t)
       angles = P.map ((tStart +) . (tStep *) . fromIntegral) [0..n]
       
  in   P.map arcPos angles
  
