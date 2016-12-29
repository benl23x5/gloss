module Graphics.Gloss.Geometry.Triangulation
where
        --TODO put exported functions here. triangulate only to be exact. pozdrav!
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (minimumBy, maximumBy, sortBy)
import Control.Applicative
import Data.Maybe

-- import Debug.Trace


type Vertices = Map.Map Vertex (Set.Set Vertex)

data Vertex
        = Vertex Point
        deriving (Show)
toPoint :: Vertex -> Point
toPoint (Vertex x) = x

instance Eq Vertex where
        (Vertex (x1,y1)) == (Vertex (x2,y2)) = abs (x1-x2) + abs (y1-y2) < 0.0001

instance Ord Vertex where
        compare v1@(Vertex (x1,y1)) v2@(Vertex (x2,y2))
                | v1 == v2 = EQ
                | otherwise = compare (x1,y1) (x2,y2)

data Edge
        = Edge
        { start :: Vertex
        , end :: Vertex }
        deriving (Show)

type Events = Map.Map Vertex (Set.Set Edge, Set.Set Edge)

instance Eq Edge where
        Edge{start = p0, end = p1} == Edge{start = p2, end = p3}
                = p0 == p2 && p1 == p3
instance Ord Edge where
        Edge{start = Vertex p0@(x0, _), end = Vertex p1@(x1, y1)} < Edge{start = Vertex p2@(x2, _), end = Vertex p3@(x3, y3)}
                            = let x = max x0 x2
                                  intersectHelper a@(ax,_) b@(bx,_) yAxisXCoorcinate
                                        = if ax == bx && ax == yAxisXCoorcinate then Just a else intersectSegVertLine a b yAxisXCoorcinate
                                  Just (_, py) = intersectHelper p0 p1 x
                                  Just (_, qy) = intersectHelper p2 p3 x
                              in if py == qy then (x1-x)*(y3-py) > (x3-x)*(y1-py) else py < qy
        compare x y
                | x == y = EQ
                | x < y = LT
                | otherwise = GT



-- Finds intersections of two Edges.
intersection :: Maybe Edge -> Maybe Edge -> Maybe Vertex
intersection (Just Edge{start = Vertex p0, end = Vertex p1}) (Just Edge{start = Vertex p2, end = Vertex p3})
                | Just x <- intersectSegSeg p0 p1 p2 p3
                , (x /= p0) && (x /= p1) && (x /= p2) && ( x /= p3)
                = Just (Vertex x)
                | otherwise = Nothing
intersection _ _ = Nothing

makeInitialVertexSet :: Path -> Vertices
makeInitialVertexSet [] = undefined
makeInitialVertexSet points = foldr accumulator Map.empty ( zip list (xs ++ list))
                                   where accumulator (p , q) acc =
                                                let insert p1 q1 = Map.insertWith Set.union p1 (Set.singleton q1)
                                                in insert p q (insert q p acc)
                                         list@(_:xs) = map Vertex points



initialEvents :: Vertices -> Events
initialEvents = Map.foldrWithKey event Map.empty

event :: Vertex -> Set.Set Vertex -> Events -> Events

event (Vertex p@(px,py)) neighbours events  = let outCompare (Vertex (x,y)) = if px == x then py < y else px < x
                                                  edgeStart point = Edge {start = Vertex p, end = point}
                                                  edgeEnd point = Edge {end = Vertex p, start = point}
                                                  startSet = Set.map edgeStart (Set.filter outCompare neighbours)
                                                  endSet = Set.map edgeEnd (Set.filter (not.outCompare) neighbours)
                                              in Map.insert (Vertex p)  (startSet, endSet) events

traverseVertices :: ((a, Map.Map b c) -> (b, c) -> (a, Map.Map b c)) -> a -> Map.Map b c -> a
traverseVertices _ acc dictionary | Map.null dictionary = acc
traverseVertices fn acc dictionary = let minElement = Map.findMin dictionary
                                         ( newAcc, newDictionary) = fn  (acc, dictionary) minElement
                                in traverseVertices fn  newAcc (Map.deleteMin newDictionary)



type AccumulatorType = (Set.Set Edge, Vertices)

accumulatorFunction :: (AccumulatorType , Events) -> (Vertex, (Set.Set Edge, Set.Set Edge)) -> (AccumulatorType, Events)

-- accumulatorFunction ((edges, vertices), events) _ | trace ("Accumulator \n" ++ show events ++ "\n\n") False = undefined
accumulatorFunction ((edges, vertices), events) (_, (startOf, endOf)) = let deletedEdges = edges `Set.difference` (startOf  `Set.union` endOf)
                                                                            minElem = lookupMin startOf
                                                                            maxElem = lookupMax startOf
                                                                            minElemLT
                                                                                | Just element <- minElem = Set.lookupLT element deletedEdges
                                                                                | otherwise = Nothing
                                                                            maxElemGT
                                                                                | Just element <- maxElem = Set.lookupGT element deletedEdges
                                                                                | otherwise = Nothing
                                                                            dIntersect = intersection minElem minElemLT
                                                                            uIntersect = intersection maxElem maxElemGT
                                                                            event1 = replaceEdge minElem dIntersect events
                                                                            event2 = replaceEdge maxElem uIntersect event1
                                                                            event5 = replaceEdge minElemLT dIntersect event2
                                                                            event6 = replaceEdge maxElemGT uIntersect event5
                                                                            event3 = insertEvent dIntersect minElem minElemLT event6
                                                                            event4 = insertEvent uIntersect maxElem maxElemGT event3
                                                                            edges1 = deletedEdges `Set.union` difference startOf minElem maxElem
                                                                            edges2 = addMaybeEdge minElem dIntersect $ addMaybeEdge maxElem uIntersect edges1
                                                                        in ((edges2, addVertex endOf vertices), event4)

addVertex :: Set.Set Edge -> Vertices -> Vertices

-- |addVertex e v  | trace ("addVertex TRACE : \n" ++ show e ++ "\n" ++ show v ++ "\n\n") False = undefined
addVertex edges vertices
                    | null edges = vertices
                    | otherwise = Set.foldr accumulator vertices edges
                                    where accumulator Edge{start = p, end = q} acc =
                                                let insert p1 q1 = Map.insertWith Set.union p1 (Set.singleton q1)
                                                in insert p q (insert q p acc)


difference :: Set.Set Edge -> Maybe Edge -> Maybe Edge -> Set.Set Edge

difference edges edge1 edge2 = let delete e s
                                    |Just x <- e = Set.delete x s
                                    |otherwise = s
                               in delete edge1 $ delete edge2 edges


addMaybeEdge :: Maybe Edge -> Maybe Vertex -> Set.Set Edge -> Set.Set Edge

addMaybeEdge (Just Edge{end = p1}) (Just p) edges = Set.insert Edge{start=p, end = p1} edges
addMaybeEdge (Just x) Nothing edges = Set.insert x edges
addMaybeEdge _ _ edges = edges

insertEvent :: Maybe Vertex -> Maybe Edge -> Maybe Edge -> Events -> Events

insertEvent (Just p) (Just Edge{start = p0, end = p1}) (Just Edge{start = p2, end = p3}) events
            = let startOf = Set.insert Edge{start = p, end = p3} $ Set.insert Edge{start = p, end = p1} Set.empty
                  endOf = Set.insert Edge{start = p0, end = p} $ Set.insert Edge{start = p2, end = p} Set.empty
              in Map.insertWith (\(x,y) (a, b) -> (x `Set.union` a, y `Set.union` b)) p (startOf, endOf) events
insertEvent _ _ _ events = events

replaceEdge :: Maybe Edge -> Maybe Vertex -> Events -> Events
replaceEdge (Just edge@Edge{start = _, end = p1}) (Just p) events = replaceEndOf p1 edge Edge{start = p, end = p1} events

replaceEdge _ _ events = events

replaceEndOf :: Vertex -> Edge -> Edge -> Events -> Events
-- e1 brisem e2 dodajem
replaceEndOf p e1 e2 events = let (startOf, endOf) = events Map.! p
                                  newEndOf = Set.insert e2 $ Set.delete e1 endOf
                              in Map.insert p  (startOf, newEndOf) events


lookupMin :: Set.Set a -> Maybe a
lookupMin set
        | Set.null set = Nothing
        | otherwise = Just $ Set.findMin set


lookupMax :: Set.Set a -> Maybe a
lookupMax set
        | Set.null set = Nothing
        | otherwise = Just $ Set.findMax set


polygonGraph :: Path -> Vertices

polygonGraph l = snd $ traverseVertices accumulatorFunction ( Set.empty, Map.empty) $ initialEvents (makeInitialVertexSet l)

pointsPositiveOrientation :: Vertex -> Vertex -> Vertex -> Ordering
pointsPositiveOrientation (Vertex (x0,y0)) (Vertex (x1,y1)) (Vertex (x2,y2)) = compare ((x1-x0)*(y2-y0)) ((x2-x0)*(y1-y0))

oriantedAngleVV :: Vector -> Vector -> Float
oriantedAngleVV v1 v2 = normalizeAngle $ argV v1 - argV v2

compareAngles :: Vertex -> Vertex -> Vertex -> Vertex -> Ordering
compareAngles (Vertex v1) (Vertex v2) (Vertex v3) (Vertex v4) = compare (oriantedAngleVV (v1-v2) (v3-v2)) (oriantedAngleVV (v1-v2) (v4-v2))

addNeighbour ::  Vertex -> Vertex -> Vertices -> Vertices
addNeighbour point1 point2 verteces = helper point2 point1 $ helper point1 point2 verteces
                          where helper p1 p2 = Map.insertWith Set.union p1 (Set.singleton p2)


removeNeighbors :: Vertex -> Vertex -> Vertices -> Vertices
removeNeighbors point1 point2 verteces = helper point2 point1 $ helper point1 point2 verteces
                          where helper p1 p2 v = let p1Set = v Map.! p1
                                                     newP1Set = Set.delete p2 p1Set
                                                 in if Set.null newP1Set then Map.delete p1 v else Map.insert p1 newP1Set v


breakUpToSimplePolygons :: Path -> [Path]
breakUpToSimplePolygons path = traversePolygonGraph Nothing [] (polygonGraph path) Set.empty

traversePolygonGraph :: Maybe Vertex -> [Path] -> Vertices -> Set.Set (Vertex, Vertex) -> [Path]
traversePolygonGraph _ paths vertices _ | Map.null vertices = paths
traversePolygonGraph Nothing paths verteces doubleEdges = let (minVertexKey, minVertexKeyValue) = Map.findMin verteces
                                                              nextVertex = maximumBy (pointsPositiveOrientation minVertexKey) minVertexKeyValue
                                                              (newVerteces, newDoubleEdges) = if Set.member (minVertexKey, nextVertex) doubleEdges
                                                                      then (verteces, Set.difference doubleEdges (Set.fromList [(minVertexKey, nextVertex),(nextVertex, minVertexKey)]))
                                                                      else (removeNeighbors minVertexKey nextVertex verteces, doubleEdges)
                                                          in traversePolygonGraph (Just minVertexKey) ([toPoint nextVertex, toPoint minVertexKey]:paths) newVerteces newDoubleEdges
traversePolygonGraph (Just firstVetex) (currentPolygon@(lastPoint:previousPoint:_):oldPolygons) verteces doubleEdges = let currentNeighbors = Set.delete (Vertex previousPoint) (verteces Map.! Vertex lastPoint)
                                                                                                                           nextVertex = minimumBy (compareAngles (Vertex previousPoint) (Vertex lastPoint)) currentNeighbors
                                                                                                                           (newVerteces, newDoubleEdges) = if Set.member (Vertex lastPoint, nextVertex) doubleEdges
                                                                                                                                   then (verteces, Set.difference doubleEdges (Set.fromList [(Vertex lastPoint, nextVertex),(nextVertex, Vertex lastPoint)]))
                                                                                                                                   else (removeNeighbors (Vertex lastPoint) nextVertex verteces, doubleEdges)
                                                                                                                       in if firstVetex == nextVertex
                                                                                                                          then traversePolygonGraph Nothing (currentPolygon:oldPolygons) newVerteces newDoubleEdges
                                                                                                                          else traversePolygonGraph (Just firstVetex) ((toPoint nextVertex:currentPolygon):oldPolygons) newVerteces newDoubleEdges
traversePolygonGraph _ _ _ _ = error "Should not be here! Wrong input proboblly."

data EventType = START | MERGE | REGULARDOWN | REGULARUP | SPLIT | END
        deriving (Show, Eq)

data MonotonEvent
        = MonotonEvent
        { eventCootdinates :: Point
        , eventType :: EventType }
        deriving (Show)

type EdgesWithHelpers = Map.Map Edge MonotonEvent

addEdge :: MonotonEvent -> MonotonEvent -> EdgesWithHelpers -> EdgesWithHelpers
addEdge p1@MonotonEvent{eventCootdinates = (x1, _)} p2@MonotonEvent{eventCootdinates = (x2, _)} edges
        | x1 == x2 = edges
        | otherwise = Map.insert (Edge (Vertex (eventCootdinates p1)) (Vertex (eventCootdinates p2))) p1 edges

findEdge :: MonotonEvent -> EdgesWithHelpers -> Maybe Edge
findEdge p edgesWithHelpers = fst Control.Applicative.<$> Map.lookupLT (Edge (Vertex (eventCootdinates p)) (Vertex (eventCootdinates p))) edgesWithHelpers

getHelper :: Edge -> EdgesWithHelpers -> MonotonEvent
getHelper = flip (Map.!)

changeHelper :: MonotonEvent -> EdgesWithHelpers -> EdgesWithHelpers
-- changeHelper p edgesWithHelpers | trace ("TRACE!!!    " ++ show p ++ "\n" ++ show edgesWithHelpers ++ "\n\n") False = undefined
changeHelper p edgesWithHelpers = let Just e = findEdge p edgesWithHelpers
                                  in Map.insert e p edgesWithHelpers

removeEdge :: MonotonEvent -> MonotonEvent -> EdgesWithHelpers -> EdgesWithHelpers
removeEdge p1 p2 = Map.delete (Edge (Vertex (eventCootdinates p1)) (Vertex (eventCootdinates p2)))

findHelper :: MonotonEvent -> EdgesWithHelpers -> MonotonEvent

findHelper p edgesWithHelpers = let Just e = findEdge p edgesWithHelpers
                                in getHelper e edgesWithHelpers

zip3Tail :: [a] -> [(a,a,a)]
zip3Tail xs = zip3 xs (tail xsForever) (tail $ tail xsForever)
                where xsForever = xs ++ xs

addNeighbourSet :: Vertex -> Vertex -> Set.Set (Vertex, Vertex) -> Set.Set (Vertex, Vertex)
addNeighbourSet v1 v2 s = Set.union s $ Set.fromList [(v1,v2), (v2,v1)]

markVerteces :: Path -> [MonotonEvent]
markVerteces points = map mapFn $ zip3Tail points
                        where mapFn (p1@(x1, _), p2@(x2, _), p3@(x3, _))
                                        | (x2 > x1 && x2 > x3) || (x2 == x1 && x2 > x3) || (x2 == x3 && x2 > x1) = if pointsPositiveOrientation (Vertex p1) (Vertex p2) (Vertex p3) == LT then MonotonEvent p2 END else MonotonEvent p2 MERGE
                                        | (x2 < x1 && x2 < x3) || (x2 == x1 && x2 < x3) || (x2 == x3 && x2 < x1) = if pointsPositiveOrientation (Vertex p1) (Vertex p2) (Vertex p3) == LT then MonotonEvent p2 START else MonotonEvent p2 SPLIT
                                        | otherwise =  if x2 > x1  then MonotonEvent p2 REGULARUP else MonotonEvent p2 REGULARDOWN

makeXMonotonGraph :: Path -> (Vertices, EdgesWithHelpers, Set.Set (Vertex,Vertex))
makeXMonotonGraph points = foldl accFn (makeInitialVertexSet points, Map.empty, Set.empty) (sortBy (\(_,a,_) (_,b,_) -> compare (eventCootdinates a) (eventCootdinates b)) $ zip3Tail $ markVerteces points)
                                where accFn (verteces, edges, doubleEdges) (perviousEvent, currentEvent@MonotonEvent {eventType = t}, nextEvent)
                                        -- | trace ("accFn Trace: " ++ show edges ++ "\n event = " ++ show currentEvent ++ "\n\n") False = undefined
                                        | t == REGULARUP = let helper = findHelper currentEvent edges
                                                               newEdges = changeHelper currentEvent edges
                                                               (newVerteces, newDoubleEdges) = connectWithMerge helper currentEvent verteces doubleEdges
                                                           in ( newVerteces , newEdges , newDoubleEdges)
                                        | t == REGULARDOWN = let e = Edge (Vertex $ eventCootdinates nextEvent) (Vertex $ eventCootdinates currentEvent)
                                                                 helper = getHelper e edges
                                                                 newEdges = addEdge currentEvent perviousEvent $ removeEdge nextEvent currentEvent edges
                                                                 (newVerteces, newDoubleEdges) = connectWithMerge helper currentEvent verteces doubleEdges
                                                             in ( newVerteces, newEdges, newDoubleEdges)
                                        | t == SPLIT = let helper = findHelper currentEvent edges
                                                           newEdges = addEdge currentEvent perviousEvent $ changeHelper currentEvent edges
                                                           newVerteces = addNeighbour (Vertex $ eventCootdinates helper) (Vertex $ eventCootdinates currentEvent) verteces
                                                           newDoubleEdges = addNeighbourSet (Vertex $ eventCootdinates helper) (Vertex $ eventCootdinates currentEvent) doubleEdges
                                                       in ( newVerteces , newEdges, newDoubleEdges)
                                        | t == END = let e = Edge (Vertex $ eventCootdinates nextEvent) (Vertex $ eventCootdinates currentEvent)
                                                         helper = if Map.null edges then Nothing else Just $ getHelper e edges
                                                         newEdges = removeEdge nextEvent currentEvent edges
                                                         (newVerteces, newDoubleEdges) = if null helper then (verteces, doubleEdges) else connectWithMerge (fromJust helper) currentEvent verteces doubleEdges
                                                     in ( newVerteces, newEdges, newDoubleEdges)
                                        | t == MERGE = let e = Edge (Vertex $ eventCootdinates nextEvent) (Vertex $ eventCootdinates currentEvent)
                                                           helper = getHelper e edges
                                                           newEdges = changeHelper currentEvent $ removeEdge nextEvent currentEvent edges
                                                           (newVerteces, newDoubleEdges) = connectWithMerge helper currentEvent verteces doubleEdges
                                                       in ( newVerteces , newEdges, newDoubleEdges)
                                        | t == START = let newEdges = addEdge currentEvent perviousEvent edges
                                                       in (verteces, newEdges, doubleEdges)
                                        | otherwise = ( verteces , edges, doubleEdges)
                                      connectWithMerge helper currentEvent verteces doubleEdges = if eventType helper == MERGE
                                                                                        then (addNeighbour (Vertex $ eventCootdinates helper) (Vertex $ eventCootdinates currentEvent) verteces, addNeighbourSet (Vertex $ eventCootdinates helper) (Vertex $ eventCootdinates currentEvent) doubleEdges)
                                                                                        else (verteces, doubleEdges)
makeXMonoton :: Path -> [Path]
makeXMonoton path = traversePolygonGraph Nothing [] graph doubleEdges
                        where (graph,_,doubleEdges) = makeXMonotonGraph path

triangulate :: Path -> [Picture]
triangulate x = map Polygon $ breakUpToSimplePolygons x
