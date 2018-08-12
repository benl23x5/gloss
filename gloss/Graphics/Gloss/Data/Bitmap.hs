
-- | Functions to load bitmap data from various places.
module Graphics.Gloss.Data.Bitmap
        ( Rectangle(..)
        , BitmapData, bitmapSize
        , BitmapFormat(..), RowOrder(..), PixelFormat(..)
        , bitmapOfForeignPtr
        , bitmapDataOfForeignPtr
        , bitmapOfByteString
        , bitmapDataOfByteString
        , bitmapOfBMP
        , bitmapDataOfBMP
        , loadBMP)
where
import Graphics.Gloss.Rendering

