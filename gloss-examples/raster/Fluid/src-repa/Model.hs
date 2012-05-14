{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Model
        ( Delta
        , Rate
        , Field
        , DensityField
        , VelocityField
        , Source        (..)
        , CurrentButton (..)

        , Model (..)
        , initModel

        , pictureOfModel
        , pixel32OfDensity
        , pixel8OfDensity

        , outputBMP
        , outputPPM)
where
import Graphics.Gloss                   
import Data.Array.Repa                  as R
import Data.Array.Repa.IO.BMP           as R
import Data.Array.Repa.Repr.ForeignPtr  as R
import Data.Bits
import Data.Word
import Unsafe.Coerce
import Prelude                          as P

-- | Time delta (seconds)
type Delta      = Float

-- | Time rate (or period) (1/seconds)
type Rate       = Float


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


-- Model ----------------------------------------------------------------------
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
initModel 
        :: Array U DIM2 Float
        -> Array U DIM2 (Float, Float)
        -> Model

initModel density velocity
 | extent density /= extent velocity
 = error "Fluid: initModel density and velocity extents do not  match"

 | otherwise
 =      Model
        { densityField   = density
        , densitySource  = Nothing
        , velocityField  = velocity
        , velocitySource = Nothing
        , clickLoc       = Nothing
        , stepsPassed    = 0
        , currButton     = None }
{-# INLINE initModel #-}


-- Picture --------------------------------------------------------------------
-- | Function to convert the Model into a Bitmap for displaying in Gloss
pictureOfModel :: Monad m => (Float, Float) -> Model -> m Picture
pictureOfModel (scaleX, scaleY) m 
 = let  (Z :. width' :. height') = R.extent $ densityField m
        width           = fromIntegral width'
        height          = fromIntegral height'

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


-- Dump -----------------------------------------------------------------------
-- Writes bitmap data to test batch-mode ran correctly
outputBMP :: DensityField -> IO ()
outputBMP df 
 = do   arr     <- computeUnboxedP $ R.map pixel8OfDensity df
        R.writeImageToBMP "./out.bmp" arr


outputPPM :: Int -> String -> Float -> Array U DIM2 Float -> IO ()
outputPPM step name scale df
 = do   let (Z :. h :. w) = extent df
        let vals          = R.toList df
        let mx            = maximum vals
        let step'         
             = replicate (4 - length (show step)) '0' P.++ show step

        let getVal x y
             =  let v     = df R.! (Z :. y :. x)
                in  truncate (v * 255 * scale)  :: Int

        let showVal x y
             =  let  v   = getVal x y
                in replicate (4 - length (show v)) ' ' P.++ show v

        let mx
             =  maximum [getVal x y | x <- [0..w-1], y <- [0..h-1]]

        let out = unlines $ 
                [ "P2"
                , show h P.++ " " P.++ show w
                , show mx]
                P.++ [ concat [ showVal x y P.++ " "
                              | x <- [0..w - 1]]
                     | y <- [h - 1, h - 2 .. 0]]

        writeFile ("out/" P.++ step' P.++ "-" P.++ name P.++ ".ppm") out




