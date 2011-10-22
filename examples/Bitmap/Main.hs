
import Graphics.Gloss
import Codec.BMP
import System.Environment

-- | Displays uncompressed 24/32 bit BMP images.
main
 = do	args	<- getArgs
	case args of
	 [fileName] -> run fileName
	 _ -> putStr 
	   $  unlines [ "usage: bitmap <file.bmp>"
		      , "  file.bmp should be a 24 or 32-bit uncompressed BMP file" ]

run fileName
 = do	(bitmap, width, height)	<- loadBitmap fileName
	displayInWindow
		fileName
		(width, height)
      		(10,  10)
      		white
      		(bitmap)


loadBitmap :: FilePath -> IO (Picture, Int,  Int)
loadBitmap fileName
 = do	eImg	<- readBMP fileName
	case eImg of
	 Left err 	-> error  $ show err
	 Right img	
	  -> let (width, height)	= bmpDimensions img
		 bytestring		= unpackBMPToRGBA32 img
	     in	 return ( Bitmap width height bytestring True
			, width, height)
	
