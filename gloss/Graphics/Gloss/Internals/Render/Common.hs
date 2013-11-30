{-# OPTIONS_HADDOCK hide #-}
module Graphics.Gloss.Internals.Render.Common where

import           Graphics.Gloss.Internals.Interface.Backend
import           Graphics.Gloss.Data.Point
import           Graphics.Rendering.OpenGL                                       (($=))
import qualified Graphics.Rendering.OpenGL.GL   as GL
import           Graphics.Rendering.OpenGL.Raw
import           Unsafe.Coerce
import           Data.IORef
import           Data.Array.Storable

-- | The OpenGL library doesn't seem to provide a nice way convert
--      a Float to a GLfloat, even though they're the same thing
--      under the covers.
--
--  Using realToFrac is too slow, as it doesn't get fused in at
--      least GHC 6.12.1
--
gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

-- | Used for similar reasons to above
gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x

-- | Renders a list of vertices using OpenGL vertex arrays
--   Used when only OpenGL ES is available
--
-- Precondition: "GL.clientState GL.VertexArray $= GL.Enabled" must have been called.
--
renderVertices :: GL.PrimitiveMode -> [Point] -> IO ()
renderVertices primMode vs
 = do
  let len = length vs
      xs  = concatMap (\(a,b) -> [gf a, gf b]) vs
  a <- newListArray (0, 2*len-1) xs
  withStorableArray (a :: StorableArray Int GL.GLfloat) $ \ptr -> do
    GL.arrayPointer GL.VertexArray $= GL.VertexArrayDescriptor 2 GL.Float 0 ptr
    GL.drawArrays primMode 0 (fromIntegral len)


-- | Perform a rendering action setting up the coords first
renderAction
        :: Backend a
        => IORef a
        -> IO ()
        -> IO ()

renderAction backendRef action
 = do
        GL.clientState GL.VertexArray $= GL.Enabled
        GL.matrixMode   $= GL.Projection
        GL.preservingMatrix
         $ do
                -- setup the co-ordinate system
                GL.loadIdentity
                (sizeX, sizeY)  <- getWindowDimensions backendRef
                let (sx, sy)    = (fromIntegral sizeX / 2, fromIntegral sizeY / 2)

                -- FIXME: sseefried: Using glOrthof which I added in OpenGLRaw
                glOrthof (-sx) sx (-sy) sy 0 (-100)

                -- draw the world
                GL.matrixMode   $= GL.Modelview 0
                action

                GL.matrixMode   $= GL.Projection

        GL.matrixMode   $= GL.Modelview 0
