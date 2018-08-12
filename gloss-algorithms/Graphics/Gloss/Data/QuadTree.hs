{-# LANGUAGE RankNTypes #-}

-- | A QuadTree can be used to recursively divide up 2D space into quadrants.
--   The smallest division corresponds to an unit `Extent`, so the total depth
--   of the tree will depend on what sized `Extent` you start with.
module Graphics.Gloss.Data.QuadTree
        ( QuadTree (..)
        , emptyTree
        , emptyNode
        , takeQuadOfTree
        , liftToQuad
        , insertByPath
        , insertByCoord
        , lookupNodeByPath
        , lookupByPath
        , lookupByCoord
        , flattenQuadTree
        , flattenQuadTreeWithExtents)
where
import Graphics.Gloss.Data.Quad
import Graphics.Gloss.Data.Extent

-- | The quad tree structure.
data QuadTree a
        -- | An empty node.
        = TNil

        -- | A leaf containint some value.
        | TLeaf a

        -- | A node with four children.
        | TNode (QuadTree a) (QuadTree a)       -- NW NE
                (QuadTree a) (QuadTree a)       -- SW SE
        deriving Show


-- | A `TNil` tree.
emptyTree :: QuadTree a
emptyTree = TNil


-- | A node with `TNil`. for all its branches.
emptyNode :: QuadTree a
emptyNode = TNode TNil TNil TNil TNil


-- | Get a quadrant from a node.
--   If the tree does not have an outer node then `Nothing`.
takeQuadOfTree
        :: Quad
        -> QuadTree a
        -> Maybe (QuadTree a)

takeQuadOfTree quad tree
 = case tree of
        TNil            -> Nothing
        TLeaf{}         -> Nothing
        TNode nw ne sw se
         -> case quad of
                NW      -> Just nw
                NE      -> Just ne
                SW      -> Just sw
                SE      -> Just se


-- | Apply a function to a quadrant of a node.
--   If the tree does not have an outer node then return the original tree.
liftToQuad
        :: Quad
        -> (QuadTree a -> QuadTree a)
        -> QuadTree a  -> QuadTree a

liftToQuad quad f tree
 = case tree of
        TNil            -> tree
        TLeaf{}         -> tree
        TNode nw ne sw se
         -> case quad of
                NW      -> TNode (f nw) ne sw se
                NE      -> TNode nw (f ne) sw se
                SW      -> TNode nw ne (f sw) se
                SE      -> TNode nw ne sw (f se)


-- | Insert a value into the tree at the position given by a path.
--   If the path intersects an existing `TLeaf` then return the original tree.
insertByPath :: [Quad] -> a -> QuadTree a -> QuadTree a

insertByPath [] x _
        = TLeaf x

insertByPath (q:qs) x tree
 = case tree of
        TNil    -> liftToQuad q (insertByPath qs x) emptyNode
        TLeaf{} -> tree
        TNode{} -> liftToQuad q (insertByPath qs x) tree


-- | Insert a value into the node containing this coordinate.
--   The node is created at maximum depth, corresponding to an unit `Extent`.
insertByCoord :: Extent -> Coord -> a -> QuadTree a -> Maybe (QuadTree a)
insertByCoord extent coord x tree
 = do   path    <- pathToCoord extent coord
        return  $  insertByPath path x tree


-- | Lookup a node based on a path to it.
lookupNodeByPath
        :: [Quad]
        -> QuadTree a
        -> Maybe (QuadTree a)

lookupNodeByPath [] tree
        = Just tree

lookupNodeByPath (q:qs) tree
 = case tree of
        TNil    -> Nothing
        TLeaf{} -> Nothing
        TNode{}
         -> let Just quad       = takeQuadOfTree q tree
            in  lookupNodeByPath qs quad


-- | Lookup an element based given a path to it.
lookupByPath :: [Quad] -> QuadTree a -> Maybe a
lookupByPath path tree
 = case lookupNodeByPath path tree of
        Just (TLeaf x)  -> Just x
        _               -> Nothing


-- | Lookup a node if a tree given a coordinate which it contains.
lookupByCoord
        :: forall a
        .  Extent       -- ^ Extent that covers the whole tree.
        -> Coord        -- ^ Coordinate of the value of interest.
        -> QuadTree a
        -> Maybe a
lookupByCoord extent coord tree
 = do   path    <- pathToCoord extent coord
        lookupByPath path tree


-- | Flatten a QuadTree into a list of its contained values, with coordinates.
flattenQuadTree
        :: forall a
        .  Extent       -- ^ Extent that covers the whole tree.
        -> QuadTree a
        -> [(Coord, a)]

flattenQuadTree extentInit treeInit
 = flatten' extentInit treeInit
 where  flatten' extent tree
         = case tree of
                TNil    -> []
                TLeaf x
                 -> let (_, s, _, w) = takeExtent extent
                    in  [((w, s), x)]

                TNode{} -> concat $ map (flattenQuad extent tree) allQuads

        flattenQuad extent tree quad
         = let  extent'         = cutQuadOfExtent quad extent
                Just tree'      = takeQuadOfTree quad tree
           in   flatten' extent' tree'


-- | Flatten a QuadTree into a list of its contained values, with extents.
flattenQuadTreeWithExtents
        :: forall a
        .  Extent       -- ^ Extent that covers the whole tree.
        -> QuadTree a
        -> [(Extent, a)]

flattenQuadTreeWithExtents extentInit treeInit
 = flatten' extentInit treeInit
 where  flatten' extent tree
         = case tree of
                TNil    -> []
                TLeaf x
                 -> [(extent, x)]

                TNode{} -> concat $ map (flattenQuad extent tree) allQuads

        flattenQuad extent tree quad
         = let  extent'         = cutQuadOfExtent quad extent
                Just tree'      = takeQuadOfTree quad tree
           in   flatten' extent' tree'


