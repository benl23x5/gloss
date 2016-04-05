{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Solver 
        ( mandelPicture
        , mandelArray
        , f2d
        , d2f)
where
import Graphics.Gloss.Raster.Array
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import Data.Word
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Bits
import GHC.Float
import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.ForeignPtr          as R
import Data.Array.Repa.Repr.HintInterleave      as R
import Data.Array.Repa.Algorithms.ColorRamp     as R
import Prelude                                  as P


mandelPicture
        :: Int          -- Window Size X
        -> Int          -- Window Size Y
        -> Int          -- Pixels X
        -> Int          -- Pixels Y
        -> Double       -- Offset X
        -> Double       -- Offset Y
        -> Double       -- zoom
        -> Double       -- radius
        -> Int          -- iterations
        -> Picture
mandelPicture winX winY pixelsX pixelsY offX offY zoom radius iters
 = let  scaleX  :: Double = 1
        scaleY  :: Double = fromIntegral winY / fromIntegral winX
   in   makePicture
                winX winY
                pixelsX pixelsY
                (mandelPixel scaleX scaleY offX offY zoom iters radius)
{-# NOINLINE mandelPicture #-}


mandelArray
        :: Int          -- Window Size X
        -> Int          -- Window Size Y
        -> Double       -- Offset X
        -> Double       -- Offset Y
        -> Double       -- zoom
        -> Double       -- radius
        -> Int          -- iterations
        -> IO (Array U DIM2 (Word8, Word8, Word8))

mandelArray winX winY offX offY zoom radius iters
 = let  scaleX  :: Double = 1
        scaleY  :: Double = fromIntegral winY / fromIntegral winX

        arr     = makeFrame winX winY 1 1
                $ mandelPixel scaleX scaleY offX offY zoom iters radius

   in   R.computeP arr
{-# NOINLINE mandelArray #-}


-- Picture --------------------------------------------------------------------
makePicture
        :: Int                  -- Window Size X
        -> Int                  -- Window Size Y
        -> Int                  -- Pixels X
        -> Int                  -- Pixels Y
        -> (Double -> Double -> Color)
        -> Picture
makePicture !winSizeX !winSizeY !zoomX !zoomY !makePixel
 = let  -- Size of the raw image to render.
        sizeX = winSizeX `div` zoomX
        sizeY = winSizeY `div` zoomY

        {-# INLINE conv #-} 
        conv (r, g, b)
         = let  r'      = fromIntegral r
                g'      = fromIntegral g
                b'      = fromIntegral b
                a       = 255 

                !w      =   unsafeShiftL r' 24
                        .|. unsafeShiftL g' 16
                        .|. unsafeShiftL b' 8
                        .|. a
           in   w

   in unsafePerformIO $ do

        -- Define the image, and extract out just the RGB color components.
        -- We don't need the alpha because we're only drawing one image.
        (arrRGB :: Array F DIM2 Word32)
                <- R.computeP  
                $ R.map conv
                $ makeFrame winSizeX winSizeY zoomX zoomY makePixel

        -- Wrap the ForeignPtr from the Array as a gloss picture.
        let picture     
                = Scale (fromIntegral zoomX) (fromIntegral zoomY)
                $ bitmapOfForeignPtr
                        sizeX sizeY     -- raw image size
                        (BitmapFormat BottomToTop PxABGR)
                        (R.toForeignPtr $ unsafeCoerce arrRGB)
                                        -- the image data.
                        False           -- don't cache this in texture memory.

        return picture
{-# INLINE makePicture #-}


-- Frame ----------------------------------------------------------------------
makeFrame
        :: Int                  -- Window Size X
        -> Int                  -- Window Size Y
        -> Int                  -- Pixels X
        -> Int                  -- Pixels Y
        -> (Double -> Double -> Color)
        -> Array (I D) DIM2 (Word8, Word8, Word8)

makeFrame !winSizeX !winSizeY !zoomX !zoomY !makePixel
 = let  -- Size of the raw image to render.
        sizeX = winSizeX `div` zoomX
        sizeY = winSizeY `div` zoomY

        fsizeX, fsizeY  :: Double
        !fsizeX          = fromIntegral sizeX
        !fsizeY          = fromIntegral sizeY

        fsizeX2, fsizeY2 :: Double
        !fsizeX2        = fsizeX / 2
        !fsizeY2        = fsizeY / 2

        -- Midpoint of image.
        midX, midY :: Int
        !midX           = sizeX `div` 2
        !midY           = sizeY `div` 2

        {-# INLINE pixelOfIndex #-}
        pixelOfIndex (Z :. y :. x)
         = let  x'      = fromIntegral (x - midX) / fsizeX2
                y'      = fromIntegral (y - midY) / fsizeY2
           in   makePixel x' y'

   in   R.hintInterleave
         $ R.map unpackColor 
         $ R.fromFunction (Z :. sizeY  :. sizeX)
         $ pixelOfIndex
{-# INLINE makeFrame #-}


-- Mandel ---------------------------------------------------------------------
mandelPixel 
        :: Double               -- Scale X
        -> Double               -- Scale Y
        -> Double               -- Offset X
        -> Double               -- Offset Y
        -> Double               -- Zoom
        -> Int                  -- iterations
        -> Double               -- max radius
        -> Double               -- X (Real)
        -> Double               -- Y (Imaginary)
        -> Color
mandelPixel scaleX scaleY x0 y0 zoom cMax rMax x y 
 = let
        !x'     = x0 + x * zoom * scaleX
        !y'     = y0 + y * zoom * scaleY

        !count  = mandelRun (fromIntegral cMax) rMax x' y'
        !v      = fromIntegral count / fromIntegral cMax

        color'
         | v > 0.99     = rgb' 0 0 0
         | (r, g, b)    <- rampColorHotToCold 0 1 v
         = rgb' r g b
   in   color'
{-# INLINE mandelPixel #-}


mandelRun :: Int -> Double -> Double -> Double -> Int
mandelRun countMax rMax cr ci
 = go cr ci 0
 where
  go :: Double -> Double -> Int -> Int
  go !zr !zi  !count
   | count >= countMax                 = count
   | sqrt (zr * zr + zi * zi) > rMax   = count

   | otherwise                          
   = let !z2r     = zr*zr - zi*zi
         !z2i     = 2 * zr * zi
         !yr      = z2r + cr
         !yi      = z2i + ci
     in  go yr yi (count + 1)
{-# INLINE mandelRun #-}


-- Conversion -----------------------------------------------------------------
f2d :: Float -> Double
f2d = float2Double
{-# INLINE f2d #-}


d2f :: Double -> Float
d2f = double2Float
{-# INLINE d2f #-}


-- | Float to Word8 conversion because the one in the GHC libraries
--   doesn't have enout specialisations and goes via Integer.
word8OfFloat :: Float -> Word8
word8OfFloat f
        = fromIntegral (truncate f :: Int) 
{-# INLINE word8OfFloat #-}


unpackColor :: Color -> (Word8, Word8, Word8)
unpackColor c
        | (r, g, b, _) <- rgbaOfColor c
        = ( word8OfFloat (r * 255)
          , word8OfFloat (g * 255)
          , word8OfFloat (b * 255))
{-# INLINE unpackColor #-}

