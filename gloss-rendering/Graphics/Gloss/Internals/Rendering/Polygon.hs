module Graphics.Gloss.Internals.Rendering.Polygon(renderComplexPolygon) where
import Graphics.Gloss.Internals.Rendering.Common
import Graphics.Rendering.OpenGL.GLU.Tessellation
import qualified Graphics.Rendering.OpenGL.GL           as GL

combiner :: a -> b -> ()
combiner a b = ()

renderComplexPolygon :: [(Float,Float)] -> IO ()
renderComplexPolygon path = do
  Triangulation ts <- triangulate TessWindingOdd 0 ( GL.Normal3 0 0 1) combiner
    (ComplexPolygon [ComplexContour [AnnotatedVertex (GL.Vertex3 (realToFrac a) (realToFrac b) 0) () | (a,b) <- path]])
  mapM_ renderTriangle ts
  return ()

renderTriangle :: Triangle a -> IO ()
renderTriangle (Triangle (AnnotatedVertex v1 _) (AnnotatedVertex v2 _) (AnnotatedVertex v3 _)) = GL.renderPrimitive GL.Polygon (mapM_ GL.vertex [v1,v2,v3])
