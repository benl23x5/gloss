{-# LANGUAGE BangPatterns #-}

-- | Rendering of continuous 2D functions as raster fields.
--
--  Gloss programs should be compiled with @-threaded@, otherwise the GHC runtime
--  will limit the frame-rate to around 20Hz.

module Graphics.Gloss.Raster.Field
        ( Display       (..)
        , animateField
        , playField)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Animate
import Data.Array.Repa                  as R
import Data.Array.Repa.Repr.ForeignPtr  as R
import Data.Word


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
   in   animate display black 
                $ (\time -> makeFrame winSizeX winSizeY zoomX zoomY (makePixel time))
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
makeFrame :: Int -> Int -> Int -> Int -> (Point -> Color) -> Picture
makeFrame !winSizeX !winSizeY !zoomX !zoomY !makePixel
 = picture
 where
        -- Size of the raw image to render.
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
           in   (x', y')
         
        -- Define the image, and extract out just the RGB color components.
        -- We don't need the alpha because we're only drawing one image.
        arrRGB :: Array D DIM2 (Float, Float, Float)
        arrRGB  = R.map (\c -> case rgbaOfColor c of 
                                (r, g, b, _) -> (r, g, b))
                $ R.fromFunction (Z :. sizeY  :. sizeX)
                $ (makePixel . pixelOfIndex)
         
         -- Convert the RGB Float colors to a flat image.
        arr8 :: Array F DIM2 Word8
        arr8    = R.computeP
                $ R.traverse
                        arrRGB
                        (\(Z :. height :. width) -> Z :. height :. width * 4)
                        (\get (Z :. y :. x) 
                         -> let (r, g, b)     = get (Z :. y :. x `div` 4)
                            in  r `seq` g `seq` b `seq`
                                case x `mod` 4 of
                                  0 -> 255
                                  1 -> word8OfFloat (b * 255)
                                  2 -> word8OfFloat (g * 255)
                                  3 -> word8OfFloat (r * 255)
                                  _ -> 0)

        -- Wrap the ForeignPtr from the Array as a gloss picture.
        picture = arr8 
                `seq` Scale (fromIntegral zoomX) (fromIntegral zoomY)
                    $ bitmapOfForeignPtr
                        sizeX sizeY             -- raw image size
                        (R.toForeignPtr arr8)   -- the image data.
                        False                   -- don't cache this in texture memory.


-- | Float to Word8 conversion because the one in the GHC libraries
--   doesn't have enout specialisations and goes via Integer.
{-# INLINE word8OfFloat #-}
word8OfFloat :: Float -> Word8
word8OfFloat f
        = fromIntegral (truncate f :: Int) 
