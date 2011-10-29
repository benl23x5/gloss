{-# LANGUAGE BangPatterns #-}
module Graphics.Gloss.Field 
        (animateFieldInWindow)
where
import Graphics.Gloss.Interface.Animate
import Data.Array.Repa                  as R
import Data.Array.Repa.Repr.ForeignPtr  as R
import Data.Word

-- TODO: add another version for a static picture
-- TODO: add another version that takes the viewport.
-- TODO: scale buffer size as window size changes.
animateFieldInWindow
        :: String                       -- window name
        -> (Int, Int)                   -- window size
        -> (Int, Int)                   -- window position
        -> (Int, Int)                   -- zoom
        -> (Float -> Point -> Color)
        -> IO ()
        
animateFieldInWindow name windowSize pos zoom pixel
 = animateInWindow
        name 
        (sizeX * zoomX, sizeY * zoomY)
        pos
        black
        mkFrame
 where
        (zoomX, zoomY)  = zoom
        (sizeX, sizeY)  = windowSize
        
        fsizeX, fsizeY  :: Float
        !fsizeX          = fromIntegral sizeX
        !fsizeY          = fromIntegral sizeY

        fsizeX2, fsizeY2 :: Float
        !fsizeX2        = fsizeX / 2
        !fsizeY2        = fsizeY / 2

        midX, midY :: Int
        !midX           = sizeX `div` 2
        !midY           = sizeY `div` 2

        mkFrame time
         = let  
                {-# INLINE pixelOfIndex #-}
                pixelOfIndex (Z :. x :. y)
                 = let  x'      = fromIntegral (x - midX) / fsizeX2
                        y'      = fromIntegral (y - midY) / fsizeY2
                   in   (x', y')
         
                -- Define the image, and extract out just the RGB color components.
                -- We don't need the alpha because we're only drawing one image.
                arrRGB :: Array D DIM2 (Float, Float, Float)
                arrRGB  = R.map (\c -> case rgbaOfColor c of 
                                        (r, g, b, _) -> (r, g, b))
                        $ R.fromFunction (Z :. sizeY :. sizeX)
                        $ (pixel time . pixelOfIndex)
         
                 -- Convert the RGB Float colors to a flat image.
                arr8 :: Array F DIM2 Word8
                arr8    = R.compute
                        $ R.traverse
                                arrRGB
                                (\(Z :. height :. width) -> Z :. height :. width * 4)
                                (\get (Z :. y :. x) 
                                 -> let (r, g, b)     = get (Z :. y :. x `div` 4)
                                    in  case x `mod` 4 of
                                          0 -> 255
                                          1 -> word8OfFloat (b * 255)
                                          2 -> word8OfFloat (g * 255)
                                          3 -> word8OfFloat (r * 255)
                                          _ -> 0)

                -- Wrap the ForeignPtr from the Array as a gloss picture.
                pic     = arr8 
                        `seq` Scale (fromIntegral zoomX) (fromIntegral zoomY)
                         $    bitmapOfForeignPtr
                                sizeX sizeY             -- raw image size
                                (R.toForeignPtr arr8)   -- the image data.
                                False                   -- don't cache this in texture memory.
           in   pic

{-# INLINE animateFieldInWindow #-}
--  INLINE so the repa functions fuse with the users client functions.


-- | Float to Word8 conversion because the one in the GHC libraries
--   doesn't have enout specialisations and goes via Integer.
{-# INLINE word8OfFloat #-}
word8OfFloat :: Float -> Word8
word8OfFloat f
        = fromIntegral (truncate f :: Int) 
