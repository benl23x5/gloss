{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Rendering.Polygon(renderComplexPolygon) where
import Graphics.Gloss.Internals.Rendering.Common
import Graphics.Rendering.OpenGL.GLU.Tessellation
import qualified Graphics.Rendering.OpenGL.GL           as GL

combiner :: a -> b -> ()
combiner _ _ = ()


-- written this way to measurably improve performance
zipLoop :: [a] -> [(a,a)]
zipLoop [] = []
zipLoop (x:xs) = go x xs where
  go y [] = [(y,x)]
  go y (z:rs) = (y,z) : go z rs

zipWithLoop :: (a->a->b) -> [a] -> [b]
zipWithLoop f = map (uncurry f) . zipLoop

-- signed angle between 2 vectors
-- https://stackoverflow.com/a/16544330/1779797
-- Note: `isConvex` would remain correct if this returned a value between theta*7/2pi and theta*7/4pi which probably provides an opportunity for optimisation
angle :: (Float,Float) -> (Float,Float) -> Float
angle (x1,y1) (x2,y2) = let dot = x1*x2 + y1*y2 -- cos theta * |v1||v2|
                            det = y2*x1 - x2*y1 -- sin theta * |v1||v2|
                        in atan2 det dot


-- Approximating 2pi by 7 is reasonable here.
-- The total rotation theoretically must be a multiple of 2pi,
-- so a little generosity doesn't break anything, but might save some cases that would fail due to floating point errors
isConvex :: [(Float,Float)] -> Bool
isConvex ps =
    -- check that it doesn't turn more than one full circle in total
    all (\theta -> (theta <= 7) && (theta > -7)) $
    scanl angleAdd 0 $ -- check that the path's direction is consistent
    zipWithLoop angle $ -- compute angles of turns at each vertex
    filter (/= (0,0)) $ -- discard edges arising from duplicated vertices
    zipWithLoop (\(x1,y1) (x2,y2) -> (x2-x1,y2-y1) ) -- produce vectors for each edge
        ps
  where
    -- Combine angles, but return a value greater than 7 if they have opposite signs
    angleAdd :: Float -> Float -> Float
    angleAdd a b = if signum a*signum b < -0.5 then 10 else a + b

renderComplexPolygon :: [(Float,Float)] -> IO ()
renderComplexPolygon path = if isConvex path
  then GL.renderPrimitive GL.Polygon $ vertexPFs path
  else do
    Triangulation ts <- triangulate TessWindingOdd 0 ( GL.Normal3 0 0 1) combiner
      (ComplexPolygon [ComplexContour [AnnotatedVertex (GL.Vertex3 (realToFrac a) (realToFrac b) 0) () | (a,b) <- path]])
    GL.renderPrimitive GL.Triangles (trisToGLVertices ts)
    return ()

trisToGLVertices ::    [Triangle a] -> IO ()
trisToGLVertices []    = return ()
trisToGLVertices ((Triangle (AnnotatedVertex v1 _) (AnnotatedVertex v2 _) (AnnotatedVertex v3 _)) : rest)
 = do   GL.vertex $ v1
        GL.vertex $ v2
        GL.vertex $ v3
        trisToGLVertices rest
{-# INLINE trisToGLVertices #-}


vertexPFs ::    [(Float, Float)] -> IO ()
vertexPFs []    = return ()
vertexPFs ((x, y) : rest)
 = do   GL.vertex $ GL.Vertex2 (gf x) (gf y)
        vertexPFs rest
{-# INLINE vertexPFs #-}
