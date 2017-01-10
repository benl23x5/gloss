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


-- | Type for representing planar graphs.
type Vertices = Map.Map Vertex (Set.Set Vertex)

-- | Whrepper for @Point@. Has different @Eq@ implmentation to absorb numerical error in calculations.
data Vertex
        = Vertex Point
        deriving (Show)

-- | Utility function to convert @Vertex@ to @Point@
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


-- | Map of events for processing. Holds segments that end in given $Vertex$ and segments that begin there.
type Events = Map.Map Vertex (Set.Set Edge, Set.Set Edge)

-- | Determen if points are close enough.
(!=) :: Point -> Point -> Bool

(!=) p1 p2 = Vertex p1 /= Vertex p2

-- | Finds intersections of two Edges.
intersection :: Maybe Edge -> Maybe Edge -> Maybe Vertex

intersection (Just Edge{start = Vertex p0, end = Vertex p1}) (Just Edge{start = Vertex p2, end = Vertex p3})
                | Just x <- intersectSegSeg p0 p1 p2 p3
                , (x != p0) && (x != p1) && (x != p2) && ( x != p3)
                = Just (Vertex x)
                | otherwise = Nothing
intersection _ _ = Nothing

-- | Turns @Path@ into a graph in witch egdes can intersect. You can think of it as a drawing of a @Path@.
makeInitialVertexSet :: Path -> Vertices

makeInitialVertexSet [] = undefined
makeInitialVertexSet points
        = foldr accumulator Map.empty ( zip list (xs ++ list))
           where accumulator (p , q) acc =
                        let insert p1 q1 = Map.insertWith Set.union p1 (Set.singleton q1)
                        in insert p q (insert q p acc)
                 list@(_:xs) = map Vertex points


-- | Calculates initial set of events.
initialEvents :: Vertices -> Events

initialEvents = Map.foldrWithKey accFn Map.empty
                where accFn (Vertex p@(px,py)) neighbours events
                        = let outCompare (Vertex (x,y)) = if px == x then py < y else px < x
                              edgeStart point = Edge {start = Vertex p, end = point}
                              edgeEnd point = Edge {end = Vertex p, start = point}
                              startSet = Set.map edgeStart (Set.filter outCompare neighbours)
                              endSet = Set.map edgeEnd (Set.filter (not.outCompare) neighbours)
                          in Map.insert (Vertex p)  (startSet, endSet) events

-- | Utility function for processing events. Similar to fold but the accumulator function also computes the new container.
traverseVertices :: ((a, Map.Map b c) -> (b, c) -> (a, Map.Map b c)) -> a -> Map.Map b c -> a

traverseVertices _ acc dictionary | Map.null dictionary = acc
traverseVertices fn acc dictionary = let minElement = Map.findMin dictionary
                                         ( newAcc, newDictionary) = fn  (acc, dictionary) minElement
                                in traverseVertices fn  newAcc (Map.deleteMin newDictionary)

-- | Accumulator type used for events processing.
type AccumulatorType = (Set.Set Edge, Vertices)

-- | Accumulator function used for events processing.
accumulatorFunction :: (AccumulatorType , Events) -> (Vertex, (Set.Set Edge, Set.Set Edge)) -> (AccumulatorType, Events)

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
                                                                            edges1 = difference deletedEdges maxElemGT minElemLT `Set.union` difference startOf minElem maxElem
                                                                            edges2 = addMaybeEdgeCliped minElem minElemLT dIntersect edges1
                                                                            edges3 = addMaybeEdgeCliped maxElem maxElemGT uIntersect edges2
                                                                            edges4 | Set.null deletedEdges =  addMaybeEdge minElem $ addMaybeEdge maxElem edges3
                                                                                   | minElem /= maxElem = addEdgeConditionaly minElem (null minElemLT ) $ addEdgeConditionaly maxElem (null maxElemGT) edges3
                                                                                   | otherwise = edges3
                                                                        in ((edges4, addVertex endOf vertices), event4)


-- | Utility function for adding an @Edge@ in a drawing represented by @Vertices@.
addVertex :: Set.Set Edge -> Vertices -> Vertices

addVertex edges vertices
        | null edges = vertices
        | otherwise = Set.foldr accumulator vertices edges
                    where accumulator Edge{start = p, end = q} acc =
                                let insert p1 q1 = Map.insertWith Set.union p1 (Set.singleton q1)
                                in insert p q (insert q p acc)

-- | Utility function for removing edges from a set if they exist.
difference :: Set.Set Edge -> Maybe Edge -> Maybe Edge -> Set.Set Edge
difference edges edge1 edge2 = let delete e s
                                    | Just x <- e = Set.delete x s
                                    | otherwise = s
                               in delete edge1 $ delete edge2 edges

-- | Utility function for adding an @Edge@ if provided and condition is @True@.
addEdgeConditionaly :: Maybe Edge -> Bool ->  Set.Set Edge -> Set.Set Edge
addEdgeConditionaly (Just x) True edges = Set.insert x edges
addEdgeConditionaly _ _ edges = edges


-- | Utility function for removing an @Edge@ from a set if provided.
addMaybeEdge :: Maybe Edge -> Set.Set Edge -> Set.Set Edge

addMaybeEdge (Just x) edges = Set.insert x edges
addMaybeEdge _ edges = edges

-- | Utility function for adding new @Edge@ to a set. Shortening the @Edge@ if @Vertex@ is provided.
addMaybeEdgeCliped :: Maybe Edge -> Maybe Edge -> Maybe Vertex -> Set.Set Edge -> Set.Set Edge
addMaybeEdgeCliped (Just Edge{start = p1}) (Just Edge{start = p2}) (Just p) edges = Set.insert Edge{start=p2, end = p} $ Set.insert Edge{start=p1, end = p} edges
addMaybeEdgeCliped (Just x) (Just _) Nothing edges = Set.insert x edges
addMaybeEdgeCliped _ _ _ edges = edges

-- | Utility function for calculating and inserting new events in @Events@ if parametars "exist".
insertEvent :: Maybe Vertex -> Maybe Edge -> Maybe Edge -> Events -> Events

insertEvent (Just p) (Just Edge{start = p0, end = p1}) (Just Edge{start = p2, end = p3}) events
            = let startOf = Set.fromList [Edge{start = p, end = p3}, Edge{start = p, end = p1}]
                  endOf = Set.fromList [Edge{start = p0, end = p}, Edge{start = p2, end = p}]
              in Map.insertWith (\(x,y) (a, b) -> (x `Set.union` a, y `Set.union` b)) p (startOf, endOf) events
insertEvent _ _ _ events = events

-- | Utility function for cutting an @Edge@ with a @Vertex@.
replaceEdge :: Maybe Edge -> Maybe Vertex -> Events -> Events

replaceEdge (Just edge@Edge{start = _, end = p1}) (Just p) events = replaceEndOf p1 edge Edge{start = p, end = p1} events
replaceEdge _ _ events = events

-- | Utility function for replacing an edge with another edge (cut version of the original edge).
replaceEndOf :: Vertex -> Edge -> Edge -> Events -> Events
replaceEndOf p e1 e2 events = let (startOf, endOf) = events Map.! p
                                  newEndOf = Set.insert e2 $ Set.delete e1 endOf
                              in Map.insert p  (startOf, newEndOf) events

-- Utility function for safe lookupMin, since set breaks when empty.
lookupMin :: Set.Set a -> Maybe a
lookupMin set
        | Set.null set = Nothing
        | otherwise = Just $ Set.findMin set

-- Utility function for safe lookupMax, since set breaks when empty.
lookupMax :: Set.Set a -> Maybe a
lookupMax set
        | Set.null set = Nothing
        | otherwise = Just $ Set.findMax set

-- | Transforms @Path@ into a planar graph with verteces containing all verteces of @Path@ and intersections of path segments.
polygonGraph :: Path -> Vertices

polygonGraph l = snd $ traverseVertices accumulatorFunction ( Set.empty, Map.empty) $ initialEvents (makeInitialVertexSet l)

-- | Utility function for checking orientation of vertices.
pointsPositiveOrientation :: Vertex -> Vertex -> Vertex -> Ordering
pointsPositiveOrientation (Vertex (x0,y0)) (Vertex (x1,y1)) (Vertex (x2,y2)) = compare ((x1-x0)*(y2-y0)) ((x2-x0)*(y1-y0))

-- | Angle between two vectors.
oriantedAngleVV :: Vector -> Vector -> Float

oriantedAngleVV v1 v2 = normalizeAngle $ argV v1 - argV v2

-- | Checking if verteces are turnign left of right.
compareAngles :: Vertex -> Vertex -> Vertex -> Vertex -> Ordering

compareAngles (Vertex v1) (Vertex v2) (Vertex v3) (Vertex v4) = compare (oriantedAngleVV (v1-v2) (v3-v2)) (oriantedAngleVV (v1-v2) (v4-v2))

-- | Utility function for adding neighbouring verteces.
addNeighbour ::  Vertex -> Vertex -> Vertices -> Vertices

addNeighbour point1 point2 verteces = helper point2 point1 $ helper point1 point2 verteces
                          where helper p1 p2 = Map.insertWith Set.union p1 (Set.singleton p2)

-- | Utility function for removing neighbouring verteces.
removeNeighbors :: (Vertex, Vertex) -> Vertices -> Vertices

removeNeighbors (point1, point2) verteces = helper point2 point1 $ helper point1 point2 verteces
                          where helper p1 p2 v = let p1Set = v Map.! p1
                                                     newP1Set = Set.delete p2 p1Set
                                                 in if Set.null newP1Set then Map.delete p1 v else Map.insert p1 newP1Set v

-- | Breaks @Path@ into a list of simple paths.
breakUpToSimplePolygons :: Path -> [Path]

breakUpToSimplePolygons path = traversePolygonGraph Nothing [] (\_ _ _ -> True) Set.empty (polygonGraph path)

-- | Traverse planar graph and calculates simple polygons that bound inner fealds of given graph.
traversePolygonGraph :: Maybe (Vertex, Vertex) -> [Path] -> (Vertices -> Vertex -> Vertex -> Bool) -> Set.Set (Vertex, Vertex) -> Vertices -> [Path]

traversePolygonGraph _ paths _ _ vertices | Map.null vertices = paths
traversePolygonGraph Nothing paths deciderFn vertexTrash vertices
        = let (minVertexKey, minVertexValue) = Map.findMin vertices
              nextVertex = maximumBy (pointsPositiveOrientation minVertexKey) minVertexValue
              newVertexTrash = if deciderFn vertices minVertexKey nextVertex then Set.insert (minVertexKey, nextVertex) vertexTrash else vertexTrash
          in traversePolygonGraph (Just (minVertexKey, nextVertex)) ([toPoint nextVertex, toPoint minVertexKey]:paths) deciderFn newVertexTrash vertices
traversePolygonGraph (Just (firstVetex, secondVertex)) (currentPolygon@(lastPoint:previousPoint:_):oldPolygons) deciderFn vertexTrash vertices
        = let currentNeighbors = Set.delete (Vertex previousPoint) (vertices Map.! Vertex lastPoint)
              nextVertex = minimumBy (compareAngles (Vertex previousPoint) (Vertex lastPoint)) currentNeighbors
              newVertexTrash = if deciderFn vertices (Vertex lastPoint) nextVertex then Set.insert (Vertex lastPoint, nextVertex) vertexTrash else vertexTrash
          in if toPoint firstVetex == lastPoint && secondVertex == nextVertex
             then let newVertices = Set.foldr removeNeighbors vertices vertexTrash
                  in traversePolygonGraph Nothing (tail currentPolygon:oldPolygons) deciderFn Set.empty newVertices
             else traversePolygonGraph (Just (firstVetex, secondVertex)) ((toPoint nextVertex:currentPolygon):oldPolygons) deciderFn newVertexTrash vertices
traversePolygonGraph _ _ _ _ _ = error "Should not be here! Wrong input proboblly."

-- | Data type for marking polygon vertices
data EventType = START | MERGE | REGULARDOWN | REGULARUP | SPLIT | END
        deriving (Show, Eq)

-- | Events for splitting polytong into x-monotone ones.
data MonotoneEvent
        = MonotoneEvent
        { eventCootdinates :: Point
        , eventType :: EventType }
        deriving (Show)

-- | Map of edges asociated with helpers.
type EdgesWithHelpers = Map.Map Edge MonotoneEvent

-- | Utility function for adding new @Edge@ to edges map.
addEdge :: MonotoneEvent -> MonotoneEvent -> EdgesWithHelpers -> EdgesWithHelpers
addEdge p1@MonotoneEvent{eventCootdinates = (x1, _)} p2@MonotoneEvent{eventCootdinates = (x2, _)} edges
        | x1 == x2 = edges
        | otherwise = Map.insert (Edge (Vertex (eventCootdinates p1)) (Vertex (eventCootdinates p2))) p1 edges

-- | Utility function for finding first edge down from given event.
findEdge :: MonotoneEvent -> EdgesWithHelpers -> Maybe Edge
findEdge p edgesWithHelpers = fst Control.Applicative.<$> Map.lookupLT (Edge (Vertex (eventCootdinates p)) (Vertex (eventCootdinates p))) edgesWithHelpers

-- | Utility function for getting helper.
getHelper :: Edge -> EdgesWithHelpers -> MonotoneEvent
getHelper = flip (Map.!)

-- | Utility function for changing helper of the edge beneath given event.
changeHelper :: MonotoneEvent -> EdgesWithHelpers -> EdgesWithHelpers
changeHelper p edgesWithHelpers = let Just e = findEdge p edgesWithHelpers
                                  in Map.insert e p edgesWithHelpers

-- | Utility function for removing an edge.
removeEdge :: MonotoneEvent -> MonotoneEvent -> EdgesWithHelpers -> EdgesWithHelpers
removeEdge p1 p2 = Map.delete (Edge (Vertex (eventCootdinates p1)) (Vertex (eventCootdinates p2)))

-- | Utility function for finging helper of the edge beneath given event.
findHelper :: MonotoneEvent -> EdgesWithHelpers -> MonotoneEvent
findHelper p edgesWithHelpers = let Just e = findEdge p edgesWithHelpers
                                in getHelper e edgesWithHelpers

-- | Utility function for making a list contatinging tupples of adjacent elements.
zip3Tail :: [a] -> [(a,a,a)]
zip3Tail xs = zip3 xs (tail xsForever) (tail $ tail xsForever)
                where xsForever = xs ++ xs

-- | Utility function for adding an edge to a set of edges.
addNeighbourSet :: Vertex -> Vertex -> Set.Set (Vertex, Vertex) -> Set.Set (Vertex, Vertex)
addNeighbourSet v1 v2 s = Set.union s $ Set.fromList [(v1,v2), (v2,v1)]

-- | Marking polyton verteces for x-monotone split.
markVerteces :: Path -> [MonotoneEvent]
markVerteces points = map mapFn $ zip3Tail points
                        where mapFn (p1@(x1, _), p2@(x2, _), p3@(x3, _))
                                        | (x2 > x1 && x2 > x3) || (x2 == x1 && x2 > x3) || (x2 == x3 && x2 > x1) = if pointsPositiveOrientation (Vertex p1) (Vertex p2) (Vertex p3) == LT then MonotoneEvent p2 END else MonotoneEvent p2 MERGE
                                        | (x2 < x1 && x2 < x3) || (x2 == x1 && x2 < x3) || (x2 == x3 && x2 < x1) = if pointsPositiveOrientation (Vertex p1) (Vertex p2) (Vertex p3) == LT then MonotoneEvent p2 START else MonotoneEvent p2 SPLIT
                                        | otherwise =  if x2 > x1  then MonotoneEvent p2 REGULARUP else MonotoneEvent p2 REGULARDOWN

-- | Makes a planar graph with fealds bouded by x-monotone polygons and a set of inner edges.
makeXMonotoneGraph :: Path -> (Vertices, EdgesWithHelpers, Set.Set (Vertex,Vertex))
makeXMonotoneGraph points = foldl accFn (makeInitialVertexSet points, Map.empty, Set.empty) (sortBy (\(_,a,_) (_,b,_) -> compare (eventCootdinates a) (eventCootdinates b)) $ zip3Tail $ markVerteces points)
                                where accFn (verteces, edges, doubleEdges) (perviousEvent, currentEvent@MonotoneEvent {eventType = t}, nextEvent)
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

-- | Breaks @Path@ into a list of x-monotone paths.
makeXMonotone :: Path -> [Path]

makeXMonotone path = traversePolygonGraph Nothing [] deciderFn Set.empty graph
                        where (graph,_,_) = makeXMonotoneGraph path
                              deciderFn vertices v1 v2 = Set.size (vertices Map.! v1) < 3 || Set.size (vertices Map.! v2) < 3

-- | Breaks x-monotone @Path@ into a list of triangles.
triangulateXMonotone :: Path -> [Path]
triangulateXMonotone points = fst $ foldl accFn ([],[]) events
                                where events = sortBy (\(_,a,_) (_,b,_) -> compare a b) $ zip3Tail points
                                      accFn (triangles, funnel@(x:y:xs)) (perviousPoint, currentPoint, nextPoint)
                                          | x == perviousPoint && (y-x) `detV` (x - currentPoint) < 0 = accFn ([y,currentPoint,x]:triangles, y:xs) (y,currentPoint, nextPoint)
                                          | x == nextPoint &&  (y-x) `detV` (x - currentPoint) > 0 = accFn ([y,currentPoint,x]:triangles, y:xs) (perviousPoint,currentPoint,y)
                                          | x /= nextPoint && x /= perviousPoint = (newTriangles currentPoint funnel ++ triangles, [currentPoint,x])
                                      accFn (triangles, l) (_, currentPoint, _) = (triangles, currentPoint : l)
                                      newTriangles point funnel = zipWith (\a b -> [a,b,point]) funnel $ tail funnel

-- | Breaks @Path@ into a list of triangles.
triangulate :: Path -> [Picture]
triangulate path = map Polygon [triangle | simplePolygon <- breakUpToSimplePolygons path
                                      , xMonotone <- makeXMonotone simplePolygon
                                      , triangle <- triangulateXMonotone xMonotone
                            ]
