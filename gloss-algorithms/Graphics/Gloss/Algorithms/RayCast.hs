{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes    #-}

-- | Various ray casting algorithms.
module Graphics.Gloss.Algorithms.RayCast
        ( castSegIntoCellularQuadTree
        , traceSegIntoCellularQuadTree)
where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Quad
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Data.QuadTree
import Data.List
import Data.Function


-- | The quadtree contains cells of unit extent (NetHack style).
--   Given a line segement (P1-P2) through the tree, get the cell
--   closest to P1 that intersects the segment, if any.
---
--   TODO: This currently uses a naive algorithm. It just calls
--         `traceSegIntoCellularQuadTree` and sorts the results
--         to get the one closest to P1. It'd be better to do a
--         proper walk over the tree in the direction of the ray.
--
castSegIntoCellularQuadTree
        :: forall a
        .  Point                        -- ^ (P1) Starting point of seg.
        -> Point                        -- ^ (P2) Final point of seg.
        -> Extent                       -- ^ Extent convering the whole tree.
        -> QuadTree a                   -- ^ The tree.
        -> Maybe (Point, Extent, a)     -- ^ Intersection point, extent of cell, value of cell (if any).

castSegIntoCellularQuadTree p1 p2 extent tree
        | cells@(_:_)   <- traceSegIntoCellularQuadTree p1 p2 extent tree
        , c : _         <- sortBy ((compareDistanceTo p1) `on` (\(a, _, _) -> a) ) cells
        = Just c

        | otherwise
        = Nothing

compareDistanceTo :: Point -> Point -> Point -> Ordering
compareDistanceTo p0 p1 p2
 = let  d1      = distance p0 p1
        d2      = distance p0 p2
   in   compare d1 d2

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2)
 = let  xd      = x2 - x1
        yd      = y2 - y1
   in   sqrt (xd * xd + yd * yd)


-- | The quadtree contains cells of unit extent (NetHack style).
--   Given a line segment (P1-P2) through the tree, return the list of cells
--   that intersect the segment.
--
traceSegIntoCellularQuadTree
        :: forall a
        .  Point                        -- ^ (P1) Starting point of seg.
        -> Point                        -- ^ (P2) Final point of seg.
        -> Extent                       -- ^ Extent covering the whole tree.
        -> QuadTree a                   -- ^ The tree.
        -> [(Point, Extent, a)]         -- ^ Intersection point, extent of cell, value of cell.

traceSegIntoCellularQuadTree p1 p2 extent tree
 = case tree of
        TNil    -> []
        TLeaf a
         -> case intersectSegExtent p1 p2 extent of
                Just pos        -> [(pos, extent, a)]
                Nothing         -> []

        TNode nw ne sw se
         | touchesSegExtent p1 p2 extent
         -> concat
             [ traceSegIntoCellularQuadTree p1 p2 (cutQuadOfExtent NW extent) nw
             , traceSegIntoCellularQuadTree p1 p2 (cutQuadOfExtent NE extent) ne
             , traceSegIntoCellularQuadTree p1 p2 (cutQuadOfExtent SW extent) sw
             , traceSegIntoCellularQuadTree p1 p2 (cutQuadOfExtent SE extent) se ]

        _ -> []
