{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Model
        ( Field
        , DensityField
        , VelocityField
        , Source        (..)
        , CurrentButton (..)

        , Model (..)
        , initModel

        , pictureOfModel
        , pixel32OfDensity
        , pixel8OfDensity)
where
import Graphics.Gloss                   
import Data.Array.Repa                  as R
import Data.Array.Repa.Repr.ForeignPtr  as R
import Foreign
import Constants
import Unsafe.Coerce
import Data.IORef

-- | A 2d field.
type Field a        
        = Array U DIM2 a

-- | Scalar density field.
type DensityField   
        = Field Float

-- | Vector velocity field.
type VelocityField  
        = Field (Float, Float)


-- | Button being pressed by the user.
data CurrentButton  
        = LeftButton
        | RightButton
        | None

-- | A density source added by the user.
data Source a
        = Source DIM2 a


-- | The world model.
data Model
        = Model
        { densityField   :: DensityField
        , densitySource  :: Maybe (Source Float)
        , velocityField  :: VelocityField
        , velocitySource :: Maybe (Source (Float, Float))
        , clickLoc       :: Maybe (Int, Int)
        , stepsPassed    :: Int
        , currButton     :: CurrentButton
        }


-- | Creates an initial blank model
initModel :: Model
initModel   
 = let  width           = unsafePerformIO $ readIORef widthArg

        density         = R.fromListUnboxed (Z:.width:.width) 
                        $ replicate (width * width) 0

        velocity        = R.fromListUnboxed (Z:.width:.width)
                        $ replicate (width * width) (0, 0)

   in   Model
        { densityField   = density
        , densitySource  = Nothing
        , velocityField  = velocity
        , velocitySource = Nothing
        , clickLoc       = Nothing
        , stepsPassed    = 0
        , currButton     = None }
{-# INLINE initModel #-}


-- | Function to convert the Model into a Bitmap for displaying in Gloss
pictureOfModel :: Monad m => Model -> m Picture
pictureOfModel m 
 = let  (Z :. width' :. height') = R.extent $ densityField m
        width           = fromIntegral width'
        height          = fromIntegral height'

        windowWidth     = unsafePerformIO $ readIORef windowWidthArg
        scaleX          = fromIntegral $ windowWidth `div` width
        scaleY          = scaleX

   in do
        (arrDensity :: Array F DIM2 Word32)
         <- computeP $ R.map pixel32OfDensity $ densityField m

        return  $ Scale scaleX scaleY 
                $ bitmapOfForeignPtr width height
                        (R.toForeignPtr $ unsafeCoerce arrDensity)
                        False
{-# NOINLINE pictureOfModel #-}


-- | Converts Float value to Word32 for pixel data
pixel32OfDensity :: Float -> Word32
pixel32OfDensity f
 = let  !fsat
          | f <  0       = 0
          | f >= 1       = 1
          | otherwise    = f

        !x      = truncate $ fsat * 255
        !a      = 255

    in   unsafeShiftL x 24 .|. unsafeShiftL x 16 
     .|. unsafeShiftL x  8 .|. a
{-# INLINE pixel32OfDensity #-}


-- | Converts Float value to a tuple of pixel components.
pixel8OfDensity :: Float -> (Word8, Word8, Word8)
pixel8OfDensity f
 = let  !fsat
          | f <  0       = 0
          | f >= 1       = 1
          | otherwise    = f

        !x      = truncate $ fsat * 255
    in  (x, x, x)
{-# INLINE pixel8OfDensity #-}


-- | Creates alpha values for display
alpha :: Array U DIM3 Word8
alpha   
 = let  width   = unsafePerformIO $ readIORef widthArg
   in   R.fromListUnboxed (Z:. width :. width :. 1) 
        $ replicate (width * width) 255
{-# INLINE alpha #-}
