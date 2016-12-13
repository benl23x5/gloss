module Graphics.Gloss.Geometry.Triangulation
        (triangulate)
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Line

type Edge = (Point, Point)

data HalfEdge
       = HalfEdge
       { origin :: Vertex
        ,twin :: HalfEdge
        ,incidentFace :: Face
        ,next :: HalfEdge
        ,previous:: HalfEdge }

data Vertex
       = Vertex
       { coord :: Point
        , incidentEdge :: HalfEdge }

data Face
       = Face
       { outerComponent :: Maybe HalfEdge
       , innerComponent :: Maybe HalfEdge }

intersection :: Edge -> Edge -> Maybe Point
intersection (p0, p1) (p2, p3)  = intersectSegSeg p0 p1 p2 p3

triangulate :: Path -> [Picture]
triangulate x = [Polygon x]


