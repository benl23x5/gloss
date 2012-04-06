{-# LANGUAGE BangPatterns, MagicHash, PatternGuards, ScopedTypeVariables #-}

-- | Rendering of continuous 2D functions as raster fields.
--
--  Gloss programs should be compiled with @-threaded@, otherwise the GHC runtime
--  will limit the frame-rate to around 20Hz.

module Graphics.Gloss.Raster.Field
        ( -- * Color
          module Graphics.Gloss.Data.Color
        , rgb, rgb8, rgb8w

          -- * Display functions
        , Display       (..)
        , Point
        , animateField
        , playField

        , convertImage)
where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Animate
import Data.Array.Repa                  as R
import Data.Array.Repa.Repr.ForeignPtr  as R
import Data.Word
import System.IO.Unsafe
import Unsafe.Coerce
import Debug.Trace
import Data.Bits

-- Color ----------------------------------------------------------------------
-- | Construct a color from red, green, blue components.
--  
--   Each component is clipped to the range [0..1]
rgb  :: Float -> Float -> Float -> Color
rgb r g b   = makeColor r g b 1.0
{-# INLINE rgb #-}


-- | Construct a color from red, green, blue components.
--
--   Each component is clipped to the range [0..255]
rgb8 :: Int -> Int -> Int -> Color
rgb8 r g b  = makeColor8 r g b 255
{-# INLINE rgb8 #-}


-- | Construct a color from red, green, blue components.
rgb8w :: Word8 -> Word8 -> Word8 -> Color
rgb8w r g b = makeColor8 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
{-# INLINE rgb8w #-}


-- Animate --------------------------------------------------------------------
-- | Animate a continuous 2D function.
animateField
        :: Display                      
                -- ^ Display mode.
        -> (Int, Int)                   
                -- ^ Pixel multiplication.
        -> (Float -> Point -> Color)    
                -- ^ Function to compute the color at a particular point.
                --
                --   It is passed the time in seconds since the program started,
                --   and a point between (-1, -1) and (+1, +1).
        -> IO ()
        
animateField display (zoomX, zoomY) makePixel
 = let  (winSizeX, winSizeY) = sizeOfDisplay display

        {-# INLINE frame #-}
        frame !time
                = return
                $ makeFrame winSizeX winSizeY zoomX zoomY (makePixel time)

   in   animateFixedIO display black frame
{-# INLINE animateField #-}
--  INLINE so the repa functions fuse with the users client functions.


-- Play -----------------------------------------------------------------------
-- | Play a game with a continous 2D function.
playField 
        :: Display                      
                -- ^ Display mode.
        -> (Int, Int)   
                -- ^ Pixel multiplication.
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
playField !display (zoomX, zoomY) !stepRate !initWorld !makePixel !handleEvent !stepWorld
 = zoomX `seq` zoomY `seq`
   let  (winSizeX, winSizeY) = sizeOfDisplay display
        
   in   winSizeX `seq` winSizeY `seq`
         play display black stepRate 
           initWorld
           (\world -> 
              world `seq` 
              makeFrame winSizeX winSizeY zoomX zoomY (makePixel world))
           handleEvent
           stepWorld
{-# INLINE playField #-}


-- Frame ----------------------------------------------------------------------
{-# INLINE sizeOfDisplay #-}
sizeOfDisplay :: Display -> (Int, Int)
sizeOfDisplay display
 = case display of
        InWindow _ s _  -> s
        FullScreen s    -> s

{-# INLINE makeFrame #-}
makeFrame 
        :: Int -> Int -> Int -> Int 
        -> (Point -> Color) -> Picture
makeFrame !winSizeX !winSizeY !zoomX !zoomY !makePixel
 = let  -- Size of the raw image to render.
        sizeX = winSizeX `div` zoomX
        sizeY = winSizeY `div` zoomY

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
        
   in unsafePerformIO $ do

        -- Define the image, and extract out just the RGB color components.
        -- We don't need the alpha because we're only drawing one image.
        traceEventIO "Gloss.Raster[makeFrame]: start frame evaluation."
        (arrRGB :: Array U DIM2 (Word8, Word8, Word8))
         <- R.computeUnboxedP
                        $ R.map unpackColor 
                        $ R.fromFunction (Z :. sizeY  :. sizeX)
                        $ pixelOfIndex

        traceEventIO "Gloss.Raster[makeFrame]: start image conversion."
        -- Convert the RGB Float colors to a flat image.
        (arr8   :: Array F DIM2 Word8)
         <- convertImage arrRGB

        traceEventIO "Gloss.Raster[makeFrame]: done, returning picture."
        -- Wrap the ForeignPtr from the Array as a gloss picture.
        let picture     = Scale (fromIntegral zoomX) (fromIntegral zoomY)
                        $ bitmapOfForeignPtr
                                sizeX sizeY             -- raw image size
                                (R.toForeignPtr arr8)   -- the image data.
                                False                   -- don't cache this in texture memory.

        return picture


-- Convert --------------------------------------------------------------------
-- | Collect RGB components into a flat array.
convertImage
        :: Monad m 
        => Array U DIM2 (Word8, Word8, Word8) 
        -> m (Array F DIM2 Word8)

convertImage arr
 = arr `deepSeqArray` 
   let  {-# INLINE conv #-} 
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
   in do 
        -- Do the writes as 32-bit then convert back to 8bit for speed.
        (arr' :: Array F DIM2 Word32)
          <- R.computeP $ R.map conv arr

        return $ unsafeCoerce arr'
{-# INLINE convertImage #-}


-- | Float to Word8 conversion because the one in the GHC libraries
--   doesn't have enout specialisations and goes via Integer.
{-# INLINE word8OfFloat #-}
word8OfFloat :: Float -> Word8
word8OfFloat f
        = fromIntegral (truncate f :: Int) 

{-# INLINE unpackColor #-}
unpackColor :: Color -> (Word8, Word8, Word8)
unpackColor c
        | (r, g, b, _) <- rgbaOfColor c
        = ( word8OfFloat (r * 255)
          , word8OfFloat (g * 255)
          , word8OfFloat (b * 255))



