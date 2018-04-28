{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Rendering of continuous 2D functions as raster fields.
--
--  Gloss programs should be compiled with @-threaded@, otherwise the GHC runtime
--  will limit the frame-rate to around 20Hz.
--
--  The performance of programs using this interface is sensitive to how much
--  boxing and unboxing the GHC simplifier manages to eliminate. For the best
--  result add INLINE pragmas to all of your numeric functions and use the following
--  compile options.
--
--  @-threaded -Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3@
--
--  See the examples the @raster@ directory of the @gloss-examples@ package
--  for more details.
--
module Graphics.Gloss.Raster.Field
        ( -- * Color
          module Graphics.Gloss.Data.Color
        , rgb,  rgbI, rgb8w
        , rgb', rgbI'

          -- * Display functions
        , Display       (..)
        , Point
        , animateField
        , playField

         -- * Frame creation
        , makePicture
        , makeFrame)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.Environment
import Data.Word
import System.IO.Unsafe
import Unsafe.Coerce
import Debug.Trace
import Data.Bits
import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.ForeignPtr          as R
import Data.Array.Repa.Repr.HintInterleave      as R
import Prelude                                  as P

-- Color ----------------------------------------------------------------------
-- | Construct a color from red, green, blue components.
--
--   Each component is clamped to the range [0..1]
rgb  :: Float -> Float -> Float -> Color
rgb r g b   = makeColor r g b 1.0
{-# INLINE rgb #-}


-- | Construct a color from red, green, blue components.
--
--   Each component is clamped to the range [0..255]
rgbI :: Int -> Int -> Int -> Color
rgbI r g b  = makeColorI r g b 255
{-# INLINE rgbI #-}


-- | Construct a color from red, green, blue components.
rgb8w :: Word8 -> Word8 -> Word8 -> Color
rgb8w r g b = makeColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
{-# INLINE rgb8w #-}


-- | Like `rgb`, but take pre-clamped components for speed.
--
--   If you're building a new color for every pixel then use this version,
--   however if your components are out of range then the picture you get will
--   be implementation dependent.
rgb' :: Float -> Float -> Float -> Color
rgb' r g b  = makeColor r g b 1.0
{-# INLINE rgb' #-}


-- | Like `rgbI`, but take pre-clamped components for speed.
--
--   If you're building a new color for every pixel then use this version,
--   however if your components are out of range then the picture you get will
--   be implementation dependent.
rgbI' :: Int -> Int -> Int -> Color
rgbI' r g b  = makeColorI r g b 255
{-# INLINE rgbI' #-}


-- Animate --------------------------------------------------------------------
-- | Animate a continuous 2D function.
animateField
        :: Display
                -- ^ Display mode.
        -> (Int, Int)
                -- ^ Number of pixels to draw per point.
        -> (Float -> Point -> Color)
                -- ^ Function to compute the color at a particular point.
                --
                --   It is passed the time in seconds since the program started,
                --   and a point between (-1, -1) and (+1, +1).
        -> IO ()

animateField display (zoomX, zoomY) makePixel
 = zoomX `seq` zoomY `seq`
 if zoomX < 1 || zoomY < 1
   then error $ "Graphics.Gloss.Raster.Field: invalid pixel scale factor "
                P.++ show (zoomX, zoomY)
   else
    do (winSizeX, winSizeY) <- sizeOfDisplay display

       let  frame !time
              = return
                $ makePicture winSizeX winSizeY zoomX zoomY (makePixel time)

       animateFixedIO display black frame (const $ return ())

{-# INLINE animateField #-}
--  INLINE so the repa functions fuse with the users client functions.

-- Play -----------------------------------------------------------------------
-- | Play a game with a continous 2D function.
playField
        :: Display
                -- ^ Display mode.
        -> (Int, Int)
                -- ^ Number of pixels to draw per point.
        -> Int  -- ^ Number of simulation steps to take
                --   for each second of real time
        -> world
                -- ^ The initial world.
        -> (world -> Point -> Color)
                -- ^ Function to compute the color of the world at the given point.
        -> (Event -> world -> world)
                -- ^ Function to handle input events.
        -> (Float -> world -> world)
                -- ^ Function to step the world one iteration.
                --   It is passed the time in seconds since the program started.
        -> IO ()
playField !display (zoomX, zoomY) !stepRate
          !initWorld !makePixel !handleEvent !stepWorld
 = zoomX `seq` zoomY `seq`
   if zoomX < 1 || zoomY < 1
     then  error $ "Graphics.Gloss.Raster.Field: invalid pixel scale factor "
                 P.++ show (zoomX, zoomY)
     else  do (winSizeX, winSizeY) <- sizeOfDisplay display
              winSizeX `seq` winSizeY `seq`
                play display black stepRate
                   ((winSizeX, winSizeY), initWorld)
                   (\((winSizeX', winSizeY'), world) ->
                      winSizeX' `seq` winSizeY' `seq` world `seq`
                      makePicture winSizeX' winSizeY' zoomX zoomY (makePixel world))
                   (\event (winSize, world) ->
                      let winSize' =
                            case event of
                              EventResize dims -> dims
                              _                -> winSize
                      in (winSize', handleEvent event world))
                   (fmap . stepWorld)
{-# INLINE playField #-}


sizeOfDisplay :: Display -> IO (Int, Int)
sizeOfDisplay display
 = case display of
        InWindow _ s _  -> return s
        FullScreen      -> getScreenSize
{-# INLINE sizeOfDisplay #-}


-- Picture --------------------------------------------------------------------
makePicture
        :: Int                  -- Window Size X
        -> Int                  -- Window Size Y
        -> Int                  -- Pixels X
        -> Int                  -- Pixels Y
        -> (Point -> Color)
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
        traceEventIO "Gloss.Raster[makePicture]: start frame evaluation."
        (arrRGB :: Array F DIM2 Word32)
                <- R.computeP
                $  R.map conv
                $  makeFrame sizeX sizeY makePixel
        traceEventIO "Gloss.Raster[makePicture]: done, returning picture."

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
        :: Int                  -- Array Size X
        -> Int                  -- Array Size Y
        -> (Point -> Color)
        -> Array (I D) DIM2 (Word8, Word8, Word8)

makeFrame !sizeX !sizeY !makePixel
 = let  -- Size of the raw image to render.
        fsizeX, fsizeY  :: Float
        !fsizeX          = fromIntegral sizeX
        !fsizeY          = fromIntegral sizeY

        fsizeX2, fsizeY2 :: Float
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
           in   makePixel (x', y')

   in   R.hintInterleave
         $ R.map unpackColor
         $ R.fromFunction (Z :. sizeY  :. sizeX)
         $ pixelOfIndex
{-# INLINE makeFrame #-}



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

