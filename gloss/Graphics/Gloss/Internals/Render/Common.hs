{-# OPTIONS_HADDOCK hide #-}
module Graphics.Gloss.Internals.Render.Common where

import qualified Graphics.Rendering.OpenGL.GL	as GL
import Unsafe.Coerce

-- | The OpenGL library doesn't seem to provide a nice way convert
--	a Float to a GLfloat, even though they're the same thing
--	under the covers.  
--
--  Using realToFrac is too slow, as it doesn't get fused in at
--	least GHC 6.12.1
--
gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

-- | Used for similar reasons to above
gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x
