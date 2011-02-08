import Codec.BMP
import Data.ByteString
import Graphics.Gloss
import System.IO.Unsafe

main
   = displayInWindow
      "Lena 202x202"
      (202, 202)
      (10,  10)
      white
      $ Scale 2 2 (unsafePerformIO bitmap)

bitmap :: IO (Picture)
bitmap
   = do
      img <- getImage
      let (width, height) = bmpDimensions img
      let bs = unpackBMPToRGBA32 img
      return $ Bitmap (fromIntegral width) (fromIntegral height) bs

-- Reads a bitmap file and outputs any errors
getImage :: IO (BMP)
getImage
   = do
      possibleImg <- readBMP "lena-101x101.bmp"
      case possibleImg of
         Left  a -> error $ show a
         Right b -> return b
