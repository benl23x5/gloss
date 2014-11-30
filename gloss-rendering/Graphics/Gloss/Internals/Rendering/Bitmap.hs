{-# OPTIONS_HADDOCK hide #-}

-- | Helper functions for rendering bitmaps
module Graphics.Gloss.Internals.Rendering.Bitmap
	( BitmapData(..)
	, reverseRGBA
	, bitmapPath
	, freeBitmapData
	)
where
import Data.Data
import Foreign


-- | Abstract 32-bit RGBA bitmap data.
data BitmapData 
        = BitmapData 
                Int                     -- length (in bytes)
                (ForeignPtr Word8)      -- pointer to data
        deriving (Eq, Data, Typeable)


instance Show BitmapData where
 show _ = "BitmapData"


-- | Generates the point path to display the bitmap centred
bitmapPath :: Float -> Float -> [(Float, Float)]
bitmapPath width height 
 = [(-width', -height'), (width', -height'), (width', height'), (-width', height')]
 where	width'  = width  / 2
	height' = height / 2


-- | Destructively reverse the byte order in an array.
--   This is necessary as OpenGL reads pixel data as ABGR, rather than RGBA
reverseRGBA :: BitmapData -> IO ()
reverseRGBA (BitmapData length8 fptr)
 = withForeignPtr fptr (reverseRGBA_ptr length8)


-- | Destructively reverses the byte order in an array.
reverseRGBA_ptr :: Int -> Ptr Word8 -> IO ()
reverseRGBA_ptr length8 ptr8
 = go (length8 `div` 4) (castPtr ptr8) 0
 where
        go :: Int -> Ptr Word32 -> Int -> IO ()
        go len ptr count
         | count < len 
         = do	curr <- peekElemOff ptr count
      	        let byte0 = shift (isolateByte0 curr) 24
      	        let byte1 = shift (isolateByte1 curr) 8
      	        let byte2 = shift (isolateByte2 curr) (-8)
      	        let byte3 = shift (isolateByte3 curr) (-24)
      	        pokeElemOff ptr count (byte0 .|. byte1 .|. byte2 .|. byte3)
      	        go len ptr (count + 1)

         | otherwise 
         = return ()

-- | Frees the allocated memory given to OpenGL to avoid a memory leak
freeBitmapData :: Ptr Word8 -> IO ()
{-# INLINE freeBitmapData #-}
freeBitmapData p = free p


-- | These functions work as bit masks to isolate the Word8 components
{-# INLINE isolateByte0 #-}
isolateByte0 :: Word32 -> Word32
isolateByte0 word =
   word .&. (255 :: Word32)

{-# INLINE isolateByte1 #-}
isolateByte1 :: Word32 -> Word32
isolateByte1 word =
   word .&. (65280 :: Word32)

{-# INLINE isolateByte2 #-}
isolateByte2 :: Word32 -> Word32
isolateByte2 word =
   word .&. (16711680 :: Word32)

{-# INLINE isolateByte3 #-}
isolateByte3 :: Word32 -> Word32
isolateByte3 word =
   word .&. (4278190080 :: Word32)
