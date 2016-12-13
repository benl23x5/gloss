module Graphics.Gloss.Geometry.Triangulation
        (triangulate)
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Line

type Edge = (Point, Point)

intersection :: Edge -> Edge -> Maybe Point
intersection (p0, p1) (p2, p3)  = intersectSegSeg p0 p1 p2 p3

triangulate :: Path -> [Picture]
triangulate x = [Polygon x]


