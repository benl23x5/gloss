module Graphics.Gloss.Geometry.Triangulation
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Line
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

--import Debug.Trace


type Vertices = Map.Map Point (Set.Set Point)

data Edge
        = Edge
        { start :: Point
        , end :: Point }
        deriving (Show)
data Event
        = Event
        { eventCoordinates :: Point
        , startOf :: Set.Set Edge
        , intersectionOf :: Set.Set Edge
        , endOf :: Set.Set Edge }
        deriving (Show)



instance Eq Event where
        Event {eventCoordinates= p0} == Event {eventCoordinates= p1} = p0 == p1

instance Ord Event where
        Event {eventCoordinates=(ax,ay)} < Event {eventCoordinates=(bx,by)} = if by == ay then ax < bx else ay < by
        compare x y
                | x == y = EQ
                | x < y = LT
                | otherwise = GT

instance Eq Edge where
        Edge{start = p0, end = p1} == Edge{start = p2, end = p3}
                = p0 == p2 && p1 == p3

instance Ord Edge where
        Edge{start = p0@(x0, y0), end = p1@(x1, y1)} < Edge{start = p2@(x2, y2), end = p3@(x3, y3)}
                = let x = max x0 x2
                      Just (_, py) = intersectSegVertLine p0 p1 x
                      Just (_, qy) = intersectSegVertLine p2 p3 x
                in if y0 == y2 then (x0 == x1) || ((x2 /= x3) && (y1 < y3)) else py < qy
        compare x y
                | x == y = EQ
                | x < y = LT
                | otherwise = GT



-- Finds intersections of two Edges.
intersection :: Edge -> Edge -> Maybe Point
intersection Edge{start = p0, end = p1}  Edge{start = p2, end = p3}
        = intersectSegSeg p0 p1 p2 p3

makeInitialVertexSet :: Path -> Vertices
makeInitialVertexSet [] = undefined
makeInitialVertexSet list@(_:xs) = foldr accumulator Map.empty ( zip list (xs ++ list))
                                   where accumulator (p , q) acc =
                                                let insert p1 q1 = Map.insertWith Set.union p1 (Set.singleton q1) 
                                                in insert p q (insert q p acc)



initialEventSet :: Vertices -> Set.Set Event
initialEventSet = Map.foldrWithKey event Set.empty

event :: Point -> Set.Set Point -> Set.Set Event -> Set.Set Event

event p@(px,py) neighbours events  = let outCompare (x,y) = if px == x then py > y else px < x
                                         edgeStart point = Edge {start = p, end = point}
                                         edgeEnd point = Edge {end = p, start = point}
                                         startSet = Set.map edgeStart (Set.filter outCompare neighbours)
                                         endSet = Set.map edgeEnd (Set.filter (not.outCompare) neighbours)
                                    in Set.insert Event{eventCoordinates = p, startOf = startSet,
                                                            intersectionOf = Set.empty, endOf = endSet} events

traverseVertices :: ((a, Set.Set b) -> b -> (a, Set.Set b)) -> a -> Set.Set b -> a
traverseVertices _ acc set | Set.null set = acc
traverseVertices fn acc set = let ( minElement, withoutMin) = Set.deleteFindMin set
                                  ( newAcc, newSet) = fn  (acc, withoutMin) minElement
                                in traverseVertices fn  newAcc newSet
-- TODO stopped here :D
-- type accumulatorType = (Set.Set Edge, Set.Set Vertex)
--
-- accumulatorFunction :: (accumulatorType , Set.Set Event) -> Event -> (accumulatorType, Set.Set Event)
-- accumulatorFunction ((edges, vertices), events) event = let deletedEdges = edges `difference` (startOf event `union` endOf event)
--



triangulate :: Path -> [Picture]
triangulate x = [Polygon x]


