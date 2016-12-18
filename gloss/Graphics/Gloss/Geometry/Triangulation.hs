module Graphics.Gloss.Geometry.Triangulation
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Line
import qualified Data.Set as Set

data Vertex
        = Vertex
        { vertexCoordinates :: Point
        , neighbours :: Set.Set Vertex }

data Edge
        = Edge
        { start :: Vertex
        , end :: Vertex }

data Event
        = Event
        { eventCoordinates :: Point
        , startOf :: Set.Set Edge
        , intersectionOf :: Set.Set Edge
        , endOf :: Set.Set Edge }


instance Eq Event where
        Event {eventCoordinates=(ax,ay)} == Event {eventCoordinates=(bx,by)} = by == ay && ax == bx

instance Ord Event where
        Event {eventCoordinates=(ax,ay)} < Event {eventCoordinates=(bx,by)} = if by == ay then ax < bx else ay < by
        compare x y = if x == y then EQ else if x < y then LT else GT

instance Eq Edge where
        (Edge {start= Vertex {vertexCoordinates=p0}, end= Vertex {vertexCoordinates=p1}}) == (Edge {start= Vertex {vertexCoordinates=p2}, end= Vertex {vertexCoordinates=p3}})
                = p0 == p2 && p1 == p3

instance Ord Edge where
        (Edge {start= Vertex {vertexCoordinates=p0@(x0,y0)}, end= Vertex {vertexCoordinates=p1@(x1,y1)}}) < (Edge {start= Vertex {vertexCoordinates=p2@(x2,y2)}, end= Vertex {vertexCoordinates=p3@(x3,y3)}})
                = let y = min y0 y2
                      Just (px, _) = intersectSegHorzLine p0 p1 y
                      Just (qx, _) = intersectSegHorzLine p2 p3 y
                in if x0 == x2 then (if y0 == y1 then True else (if y2 == y3 then False else x1 < x3)) else px < qx
        compare x y = if x == y then EQ else if x < y then LT else GT



-- Finds intersections of two Edges.
intersection :: Edge -> Edge -> Maybe Point
intersection (Edge {start= Vertex {vertexCoordinates=p0}, end= Vertex {vertexCoordinates=p1}})  (Edge {start= Vertex {vertexCoordinates=p2}, end= Vertex {vertexCoordinates=p3}})
        = intersectSegSeg p0 p1 p2 p3

triangulate :: Path -> [Picture]
triangulate x = [Polygon x]


