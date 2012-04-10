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
        , floatToWord8
        )
where
import Graphics.Gloss                   
import Data.Array.Repa                  as R
import Data.Array.Repa.Repr.ForeignPtr  as R
import Foreign
import Constants
import Unsafe.Coerce

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
 = let  density         = R.fromListUnboxed (Z:.widthI:.widthI) 
                        $ replicate (widthI * widthI) 0

        velocity        = R.fromListUnboxed (Z:.widthI:.widthI)
                        $ replicate (widthI*widthI) (0, 0)

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

        {-# INLINE conv #-} 
        conv x
         = let  x'      = floatToWord8 x
                a       = 255 

                !w      =   unsafeShiftL x' 24
                        .|. unsafeShiftL x' 16
                        .|. unsafeShiftL x' 8
                        .|. a
           in   w

   in do
        (arrDensity :: Array F DIM2 Word32)
         <- computeP $ R.map conv $ densityField m

        return  $ Scale scaleX scaleY 
                $ bitmapOfForeignPtr width height
                        (R.toForeignPtr $ unsafeCoerce arrDensity)
                        False


-- | Converts Float value to Word8 for pixel data
floatToWord8 :: Float -> Word32
floatToWord8 f
        | f <  0.0  = 0
        | f >= 1.0  = 255
        | otherwise = truncate $ f * 255
{-# INLINE floatToWord8 #-}


-- | Creates alpha values for display
alpha :: Array U DIM3 Word8
alpha   = R.fromListUnboxed (Z:. widthI :. widthI :. 1) 
        $ replicate (widthI*widthI) 255
{-# INLINE alpha #-}
