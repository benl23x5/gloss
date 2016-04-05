{-# OPTIONS_HADDOCK hide #-}

-- | Helper functions for rendering bitmaps
module Graphics.Gloss.Internals.Rendering.Bitmap
        ( BitmapData(..)
        , BitmapFormat(..), PixelFormat(..), RowOrder(..)
        , bitmapPath
        , freeBitmapData)
where
import Data.Data
import Foreign


-- | Abstract 32-bit RGBA bitmap data.
data BitmapData
        = BitmapData
        { bitmapDataLength :: Int  -- length (in bytes)
        , bitmapFormat     :: BitmapFormat
        , bitmapPointer    :: (ForeignPtr Word8) } 
        deriving (Eq, Data, Typeable)


-- | Description of how the bitmap is layed out in memory.
--
--   * Prior version of Gloss assumed `BitmapFormat BottomToTop PxAGBR`
--
data BitmapFormat
        = BitmapFormat 
        { rowOrder    :: RowOrder
        , pixelFormat :: PixelFormat }
        deriving (Eq, Data, Typeable, Show, Ord)


-- | Order of rows in an image are either:
--
--   * `TopToBottom` - the top row, followed by the next-lower row and so on.
--   * `BottomToTop` - the bottom row followed by the next-higher row and so on.
--
data RowOrder
        = TopToBottom 
        | BottomToTop
        deriving (Eq, Data, Typeable, Show, Ord, Enum, Bounded)


-- | Pixel formats describe the order of the color channels in memory.
data PixelFormat
        = PxRGBA | PxABGR
        deriving (Eq, Data, Typeable, Show, Ord, Enum, Bounded)


instance Show BitmapData where
 show _ = "BitmapData"


-- | Generates the point path to display the bitmap centred
bitmapPath :: Float -> Float -> [(Float, Float)]
bitmapPath width height
 = [(-width', -height'), (width', -height'), (width', height'), (-width', height')]
 where  width'  = width  / 2
        height' = height / 2


-- | Frees the allocated memory given to OpenGL to avoid a memory leak
freeBitmapData :: Ptr Word8 -> IO ()
freeBitmapData p = free p
{-# INLINE freeBitmapData #-}
