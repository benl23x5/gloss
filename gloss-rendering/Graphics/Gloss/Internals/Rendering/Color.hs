{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Rendering.Color where
import Graphics.Gloss.Internals.Data.Color
import Unsafe.Coerce
import qualified Graphics.Rendering.OpenGL.GL		as GL


-- | Convert one of our Colors to OpenGL's representation.
glColor4OfColor :: Fractional a => Color -> GL.Color4 a
glColor4OfColor color
 = case color of
	RGBA r g b a
	 -> let	rF	= unsafeCoerce r
		gF	= unsafeCoerce g
		bF	= unsafeCoerce b
		aF	= unsafeCoerce a
   	    in	GL.Color4 rF gF bF aF
{-# INLINE glColor4OfColor #-}
