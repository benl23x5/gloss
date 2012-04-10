{-# LANGUAGE ScopedTypeVariables #-}
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
                        $ replicate (widthI*widthI) (0,0)

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

        word8M          = R.map floatToWord8 $ densityField m
        convertedM      = extend (Z :. All :. All :. (3::Int)) word8M

   in do
        (arrDensity :: Array F DIM3 Word8)
         <- computeP $ convertedM R.++ alpha

        return  $ Scale scaleX scaleY 
                $ bitmapOfForeignPtr width height
                        (R.toForeignPtr arrDensity)
                        False


-- | Converts Float value to Word8 for pixel data
floatToWord8 :: Float -> Word8
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
