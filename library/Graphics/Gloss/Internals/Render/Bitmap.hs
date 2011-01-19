{-# OPTIONS -fwarn-incomplete-patterns #-}

module Graphics.Gloss.Internals.Render.Bitmap
   ( reverseRGBA )
where

import Foreign

-- This is necessary as openGL reads pixel data as ABGR, rather than RGBA
reverseRGBA :: [Word8] -> [Word8]
reverseRGBA (a:b:c:d:rest) =
	[d,c,b,a] ++ (reverseRGBA rest)
reverseRGBA [] =
	[]
reverseRGBA _ =
	error "Incorrect image data given: number of pixel channels is not divisable by 4."

