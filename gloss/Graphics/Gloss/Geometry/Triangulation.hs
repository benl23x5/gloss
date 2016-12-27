module Graphics.Gloss.Geometry.Triangulation
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Line
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace

--import Debug.Trace


type Vertices = Map.Map Point (Set.Set Point)

data Edge
        = Edge
        { start :: Point
        , end :: Point }
        deriving (Show)

type Events = Map.Map Point (Set.Set Edge, Set.Set Edge)
-- data Event
--         = Event
--         { eventCoordinates :: Point
--         , startOf :: Set.Set Edge
--         , endOf :: Set.Set Edge }
--         deriving (Show)



-- instance Eq Event where
--         Event {eventCoordinates= p0} == Event {eventCoordinates= p1} = p0 == p1

-- instance Ord Event where
--         Event {eventCoordinates=(ax,ay)} < Event {eventCoordinates=(bx,by)} = if by == ay then ax < bx else ay < by
--         compare x y
--                 | x == y = EQ
--                 | x < y = LT
--                 | otherwise = GT

instance Eq Edge where
        Edge{start = p0, end = p1} == Edge{start = p2, end = p3}
                = p0 == p2 && p1 == p3
-- TODO fix when one of segments are vertical
instance Ord Edge where
        Edge{start = p0@(x0, y0), end = p1@(x1, y1)} < Edge{start = p2@(x2, y2), end = p3@(x3, y3)}
                = let x = max x0 x2
                      intersectHelper a@(ax,_) b@(bx,_) x = if ax == bx && ax == x then Just a else intersectSegVertLine a b x
                      Just (_, py) = intersectHelper p0 p1 x
                      Just (_, qy) = intersectHelper p2 p3 x
                in if py == qy then (x1-x)*(y3-py) > (x3-x)*(y1-py) else py < qy
        compare x y
                | x == y = EQ
                | x < y = LT
                | otherwise = GT



-- Finds intersections of two Edges.
intersection :: Maybe Edge -> Maybe Edge -> Maybe Point
intersection (Just Edge{start = p0, end = p1}) (Just Edge{start = p2, end = p3})
                | Just x <- intersectSegSeg p0 p1 p2 p3
                , (x /= p0) && (x /= p1) && (x /= p2) && ( x /= p3)
                = Just x
                | otherwise = Nothing
intersection _ _ = Nothing

makeInitialVertexSet :: Path -> Vertices
makeInitialVertexSet [] = undefined
makeInitialVertexSet list@(_:xs) = foldr accumulator Map.empty ( zip list (xs ++ list))
                                   where accumulator (p , q) acc =
                                                let insert p1 q1 = Map.insertWith Set.union p1 (Set.singleton q1) 
                                                in insert p q (insert q p acc)



initialEvents :: Vertices -> Events
initialEvents = Map.foldrWithKey event Map.empty

event :: Point -> Set.Set Point -> Events -> Events

event p@(px,py) neighbours events  = let outCompare (x,y) = if px == x then py < y else px < x
                                         edgeStart point = Edge {start = p, end = point}
                                         edgeEnd point = Edge {end = p, start = point}
                                         startSet = Set.map edgeStart (Set.filter outCompare neighbours)
                                         endSet = Set.map edgeEnd (Set.filter (not.outCompare) neighbours)
                                    in Map.insert p  (startSet, endSet) events

traverseVertices :: ((a, Map.Map b c) -> (b, c) -> (a, Map.Map b c)) -> a -> Map.Map b c -> a
traverseVertices _ acc dictionary | Map.null dictionary = acc
traverseVertices fn acc dictionary = let ( minElement, withoutMin) = Map.deleteFindMin dictionary
                                         ( newAcc, newDictionary) = fn  (acc, dictionary) minElement
                                in traverseVertices fn  newAcc (Map.deleteMin newDictionary)



type AccumulatorType = (Set.Set Edge, Vertices)

accumulatorFunction :: (AccumulatorType , Events) -> (Point, (Set.Set Edge, Set.Set Edge)) -> (AccumulatorType, Events)

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


addMaybeEdge :: Maybe Edge -> Maybe Point -> Set.Set Edge -> Set.Set Edge

addMaybeEdge (Just Edge{end = p1}) (Just p) edges = Set.insert Edge{start=p, end = p1} edges
addMaybeEdge (Just x) Nothing edges = Set.insert x edges
addMaybeEdge _ _ edges = edges

insertEvent :: Maybe Point -> Maybe Edge -> Maybe Edge -> Events -> Events

insertEvent (Just p) (Just Edge{start = p0, end = p1}) (Just Edge{start = p2, end = p3}) events 
            = let startOf = Set.insert Edge{start = p, end = p3} $ Set.insert Edge{start = p, end = p1} Set.empty
                  endOf = Set.insert Edge{start = p0, end = p} $ Set.insert Edge{start = p2, end = p} Set.empty
              in Map.insertWith (\(x,y) (a, b) -> (x `Set.union` a, y `Set.union` b)) p (startOf, endOf) events
insertEvent _ _ _ events = events           

replaceEdge :: Maybe Edge -> Maybe Point -> Events -> Events

-- replaceEdge e p events | trace ("replaceEdge TRACE  \n" ++ show e ++ "\n" ++ show p ++ "\n" ++ show events ++"\n\n") False = undefined
-- replaceEdge (Just edge@Edge{start = p0, end = p1}) (Just p) events = let e = replaceEndOf p0 edge Edge{start = p0, end = p} events
--                                                                      in replaceStartOf p1 edge Edge{start = p, end = p1} e
replaceEdge (Just edge@Edge{start = p0, end = p1}) (Just p) events = replaceEndOf p1 edge Edge{start = p, end = p1} events

replaceEdge _ _ events = events

replaceStartOf :: Point -> Edge -> Edge -> Events -> Events
-- e1 brisem e2 dodajem
-- replaceStartOf p q r t  | trace ("replaceStartOf " ++ (show p) ++ " " ++ (show q) ++ " " ++ (show r)++  "\n" ++ show t) False = undefined
replaceStartOf p e1 e2 events = let (startOf, endOf) = events Map.! p
                                    newStartOf = Set.insert e2 $ Set.delete e1 startOf
                                in Map.insert p  (newStartOf, endOf) events

replaceEndOf :: Point -> Edge -> Edge -> Events -> Events
-- e1 brisem e2 dodajem
-- replaceEndOf p q r t  | trace ("replaceEndOf " ++ (show p) ++ "\n" ++ (show q) ++ "\n" ++ (show r) ++ "\n" ++ show t++"\n\n") False = undefined
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

pointsPositiveOrientation :: Point -> Point -> Point -> Bool
pointsPositiveOrientation (x0,y0) (x1,y1) (x2,y2) = (x1-x0)*(y2-y0) > (x2-x0)*(y1-y0)


-- breakUpToSimplePolygons :: Path -> [Path]


triangulate :: Path -> [Picture]
triangulate x = [Polygon x]


