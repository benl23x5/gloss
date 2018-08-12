{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Rendering of Repa arrays as raster images.
--
--  Gloss programs should be compiled with @-threaded@, otherwise the GHC runtime
--  will limit the frame-rate to around 20Hz.
--
--  The performance of programs using this interface is sensitive to how much
--  boxing and unboxing the GHC simplifier manages to eliminate. For the best
--  result add INLINE pragmas to all of your numeric functions and use the following
--  compile options.
--
--  @-threaded -Odph -fno-liberate-case -funfolding-use-threshold1000
--   -funfolding-keeness-factor1000 -fllvm -optlo-O3@
--
--  See the examples the @raster@ directory of the @gloss-examples@ package
--  for more details.
--
module Graphics.Gloss.Raster.Array
        ( -- * Color
          module Graphics.Gloss.Data.Color
        , rgb,  rgbI, rgb8w
        , rgb', rgbI'

          -- * Display functions
        , Display       (..)
        , animateArray
        , playArray
        , animateArrayIO
        , playArrayIO)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Rendering
import Data.Word
import System.IO.Unsafe
import Unsafe.Coerce
import Debug.Trace
import Data.Bits
import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.ForeignPtr          as R
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
rgb8w r g b = makeRawColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
{-# INLINE rgb8w #-}


-- | Like `rgb`, but take pre-clamped components for speed.
--
--   If you're building a new color for every pixel then use this version,
--   however if your components are out of range then the picture you get will
--   be implementation dependent.
rgb' :: Float -> Float -> Float -> Color
rgb' r g b  = makeRawColor r g b 1.0
{-# INLINE rgb' #-}


-- | Like `rgbI`, but take pre-clamped components for speed.
--
--   If you're building a new color for every pixel then use this version,
--   however if your components are out of range then the picture you get will
--   be implementation dependent.
rgbI' :: Int -> Int -> Int -> Color
rgbI' r g b  = makeRawColorI r g b 255
{-# INLINE rgbI' #-}


-- Animate --------------------------------------------------------------------
-- | Animate a bitmap generated from a Repa array.
animateArray
        :: Display
                -- ^ Display mode.
        -> (Int, Int)
                -- ^ Number of pixels to draw per element.
        -> (Float -> Array D DIM2 Color)
                -- ^ A function to construct a delayed array for the given time.
                --   The function should return an array of the same extent each
                --   time it is applied.
                --
                --   It is passed the time in seconds since the program started.
        -> IO ()

animateArray display scale@(scaleX, scaleY) makeArray
 = scaleX `seq` scaleY `seq`
 if scaleX < 1 || scaleY < 1
   then error $ "Graphics.Gloss.Raster.Array: invalid pixel scale factor "
                P.++ show (scaleX, scaleY)
   else let {-# INLINE frame #-}
            frame !time          = return $ makeFrame scale (makeArray time)
        in  animateFixedIO display black frame (const $ return ())
{-# INLINE animateArray #-}
--  INLINE so the repa functions fuse with the users client functions.


-- AnimateIO --------------------------------------------------------------------
-- | Animate a bitmap generated from a Repa array, via the IO monad.
animateArrayIO
        :: Display
                -- ^ Display mode.
        -> (Int, Int)
                -- ^ Number of pixels to draw per element.
        -> (Float -> IO (Array D DIM2 Color))
                -- ^ A function to construct a delayed array for the given time.
                --   The function should return an array of the same extent each
                --   time it is applied.
                --
                --   It is passed the time in seconds since the program started.
        -> IO ()

animateArrayIO display scale@(scaleX, scaleY) makeArray
 = scaleX `seq` scaleY `seq`
 if scaleX < 1 || scaleY < 1
   then error $ "Graphics.Gloss.Raster.Array: invalid pixel scale factor "
                P.++ show (scaleX, scaleY)
   else let {-# INLINE frame #-}
            frame !time          = fmap (makeFrame scale) (makeArray time)
        in  animateFixedIO display black frame (const $ return ())
{-# INLINE animateArrayIO #-}
--  INLINE so the repa functions fuse with the users client functions.


-- Play -----------------------------------------------------------------------
-- | Play with a bitmap generated from a Repa array.
playArray
        :: Display
                -- ^ Display mode.
        -> (Int, Int)
                -- ^ Number of pixels to draw per element.
        -> Int  -- ^ Number of simulation steps to take
                --   for each second of real time
        -> world
                -- ^ The initial world.
        -> (world -> Array D DIM2 Color)
                -- ^ Function to convert the world to an array.
        -> (Event -> world -> world)
                -- ^ Function to handle input events.
        -> (Float -> world -> world)
                -- ^ Function to step the world one iteration.
                --   It is passed the time in seconds since the program started.
        -> IO ()
playArray !display scale@(scaleX, scaleY) !stepRate
          !initWorld !makeArray !handleEvent !stepWorld
 = scaleX `seq` scaleY `seq`
   if scaleX < 1 || scaleY < 1
     then  error $ "Graphics.Gloss.Raster.Array: invalid pixel scale factor "
                 P.++ show scale
     else  let  {-# INLINE frame #-}
                frame !world    = makeFrame scale (makeArray world)

           in   play display black
                        stepRate
                        initWorld
                        frame
                        handleEvent
                        stepWorld
{-# INLINE playArray #-}


-- PlayIO -----------------------------------------------------------------------
-- | Play with a bitmap generated from a Repa array, via the IO monad.
playArrayIO
        :: Display
                -- ^ Display mode.
        -> (Int, Int)
                -- ^ Number of pixels to draw per element.
        -> Int  -- ^ Number of simulation steps to take
                --   for each second of real time
        -> world
                -- ^ The initial world.
        -> (world -> IO (Array D DIM2 Color))
                -- ^ Function to convert the world to an array.
        -> (Event -> world -> IO world)
                -- ^ Function to handle input events.
        -> (Float -> world -> IO world)
                -- ^ Function to step the world one iteration.
                --   It is passed the time in seconds since the program started.
        -> IO ()
playArrayIO !display scale@(scaleX, scaleY) !stepRate
            !initWorld !makeArray !handleEvent !stepWorld
 = scaleX `seq` scaleY `seq`
   if scaleX < 1 || scaleY < 1
     then  error $ "Graphics.Gloss.Raster.Array: invalid pixel scale factor "
                 P.++ show scale
     else  let  {-# INLINE frame #-}
                frame !world    = fmap (makeFrame scale) (makeArray world)

           in  playIO display black
                        stepRate
                        initWorld
                        frame
                        handleEvent
                        stepWorld
{-# INLINE playArrayIO #-}


-- Frame ----------------------------------------------------------------------
makeFrame :: (Int, Int) -> Array D DIM2 Color -> Picture
makeFrame (scaleX, scaleY) !array
 = let  -- Size of the array
        _ :. sizeY :. sizeX
                         = R.extent array

        convColor :: Color -> Word32
        convColor color
         = let  (r, g, b) = unpackColor color
                r'        = fromIntegral r
                g'        = fromIntegral g
                b'        = fromIntegral b
                a         = 255

                !w        =  unsafeShiftL r' 24
                         .|. unsafeShiftL g' 16
                         .|. unsafeShiftL b' 8
                         .|. a
           in   w
        {-# INLINE convColor #-}

   in unsafePerformIO $ do

        -- Define the image, and extract out just the RGB color components.
        -- We don't need the alpha because we're only drawing one image.
        traceEventIO "Gloss.Raster[makeFrame]: start frame evaluation."
        (arrRGB :: Array F DIM2 Word32)
                <- R.computeP $ R.map convColor array
        traceEventIO "Gloss.Raster[makeFrame]: done, returning picture."

        -- Wrap the ForeignPtr from the Array as a gloss picture.
        let picture
                = Scale (fromIntegral scaleX) (fromIntegral scaleY)
                $ bitmapOfForeignPtr
                        sizeX sizeY     -- raw image size
                        (BitmapFormat BottomToTop PxABGR)
                        (R.toForeignPtr $ unsafeCoerce arrRGB)
                                        -- the image data.
                        False           -- don't cache this in texture memory.

        return picture
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


sizeOfDisplay :: Display -> IO (Int, Int)
sizeOfDisplay display
 = case display of
        InWindow _ s _  -> return s
        FullScreen      -> getScreenSize
{-# INLINE sizeOfDisplay #-}

