module Graphics.Gloss.Geometry.Triangulation
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Line
import qualified Data.Set as Set
import Data.Maybe
--import Debug.Trace

data Vertex
        = Vertex
        { vertexCoordinates :: Point
        , neighbours :: Set.Set Point }
        deriving (Show)
data Edge
        = Edge
        { start :: Vertex
        , end :: Vertex }
        deriving (Show)
data Event
        = Event
        { eventCoordinates :: Point
        , startOf :: Set.Set Edge
        , intersectionOf :: Set.Set Edge
        , endOf :: Set.Set Edge }
        deriving (Show)

instance Eq Vertex where
        Vertex {vertexCoordinates = p0} == Vertex {vertexCoordinates = p1} = p0 == p1

instance Ord Vertex where
        Vertex {vertexCoordinates=(ax,ay)} < Vertex {vertexCoordinates=(bx,by)} = if by == ay then ax < bx else ay < by
        compare x y = if x == y then EQ else if x < y then LT else GT


instance Eq Event where
        Event {eventCoordinates= p0} == Event {eventCoordinates= p1} = p0 == p1

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

makeInitialVertexSet :: Path -> Set.Set Vertex
makeInitialVertexSet [] = undefined
makeInitialVertexSet list@(_:xs) = foldr accumulator Set.empty ( zip list (xs ++ list))
                                   where accumulator (p , q ) acc =
                                                let pm = getVertex p acc
                                                    qm = getVertex q acc
                                                    pneig = if (null pm) then Set.empty else (neighbours (fromJust pm))
                                                    qneig = if (null qm) then Set.empty else (neighbours (fromJust qm))
                                                in insertVertices [Vertex {vertexCoordinates = p, neighbours = Set.insert q pneig}, Vertex {vertexCoordinates = q, neighbours = Set.insert p qneig}] acc



insertVertices :: [Vertex] -> Set.Set Vertex -> Set.Set Vertex
insertVertices = Set.union . Set.fromList

getVertex :: Point -> Set.Set Vertex -> Maybe Vertex
getVertex p set
        | null (index ) = Nothing
        | otherwise  = Just (Set.elemAt (fromJust index) set)
        where index = Set.lookupIndex (Vertex {vertexCoordinates = p, neighbours = Set.empty}) set


initialEventSet :: Set.Set Vertex -> Set.Set Event
initialEventSet set = Set.mapMonotonic (event set) set

event :: Set.Set Vertex -> Vertex -> Event

event set v@(Vertex { vertexCoordinates = p@(px,py) , neighbours = list}) = let outCompare (x,y) = if (py == y) then px < x else py > y
                                                                                edgeStart point = Edge {start = v, end = (fromJust (getVertex point set))}
                                                                                edgeEnd point = Edge {end = v, start = (fromJust (getVertex point set))}
                                                                                startSet = Set.map edgeStart (Set.filter outCompare list)
                                                                                endSet = Set.map edgeEnd (Set.filter (not.outCompare) list)
                                                        in Event {eventCoordinates = p, startOf = startSet, intersectionOf = Set.empty , endOf = endSet}

traverseVertices :: ((a, Set.Set b) -> b -> (a, Set.Set b)) -> a -> Set.Set b -> a
traverseVertices _ start set | Set.null set = start
traverseVertices fn start set = let (min, withoutMin) = deleteFindMin set
                                    (newStart, newSet) = fn (start, withoutMin) min
                                in traverseVertices fn newStart newSet
-- TODO stopped here :D
-- type accumulatorType = (Set.Set Edge, Set.Set Vertex)
--
-- accumulatorFunction :: (accumulatorType , Set.Set Event) -> Event -> (accumulatorType, Set.Set Event)
-- accumulatorFunction ((edges, vertices), events) event = let deletedEdges = edges `difference` (startOf event `union` endOf event)
--



triangulate :: Path -> [Picture]
triangulate x = [Polygon x]


