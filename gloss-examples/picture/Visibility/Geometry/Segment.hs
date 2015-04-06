
module Geometry.Segment
        ( Segment
        , translateSegment
        , splitSegmentsOnY
        , splitSegmentsOnX
        , chooseSplitX)
where
import Graphics.Gloss
import Graphics.Gloss.Geometry.Line
import Data.Maybe
import Data.Function
import qualified Data.Vector.Unboxed    as V

-- | A line segement in the 2D plane.
type Segment    = (Int, (Float, Float), (Float, Float))


-- | Translate both endpoints of a segment.
translateSegment :: Float -> Float -> Segment -> Segment
translateSegment tx ty (n, (x1, y1), (x2, y2))
        = (n, (x1 + tx, y1 + ty), (x2 + tx, y2 + ty))
        

-- | Split segments that cross the line y = y0, for some y0.
splitSegmentsOnY :: Float -> V.Vector Segment -> V.Vector Segment
splitSegmentsOnY y0 segs
 = let  
        -- TODO: we only need to know IF the seg crosse the line here,
        --       not the actual intersection point. Do a faster test.
        (segsCross, segsOther)
                = V.unstablePartition 
                        (\(_, p1, p2) -> isJust $ intersectSegHorzLine p1 p2 y0)
                        segs

        -- TODO: going via lists here is bad.
        splitCrossingSeg :: Segment -> V.Vector Segment
        splitCrossingSeg (n, p1, p2)
         = let  Just pCross     = intersectSegHorzLine p1 p2 y0
           in   V.fromList [(n, p1, pCross), (n, pCross, p2)]
        
        -- TODO: vector append requires a copy. 
   in   segsOther V.++ (V.concat $ map splitCrossingSeg $ V.toList segsCross)


-- | Split segments that cross the line x = x0, for some x0.
splitSegmentsOnX :: Float -> V.Vector Segment -> V.Vector Segment
splitSegmentsOnX x0 segs
 = let  
        -- TODO: we only need to know IF the seg crosse the line here,
        --       not the actual intersection point. Do a faster test.
        (segsCross, segsOther)
                = V.unstablePartition 
                        (\(_, p1, p2) -> isJust $ intersectSegVertLine p1 p2 x0)
                        segs

        -- TODO: going via lists here is bad.
        splitCrossingSeg :: Segment -> V.Vector Segment
        splitCrossingSeg (n, p1, p2)
         = let  Just pCross     = intersectSegVertLine p1 p2 x0
           in   V.fromList [(n, p1, pCross), (n, pCross, p2)]
        
        -- TODO: vector append requires a copy. 
   in   segsOther V.++ (V.concat $ map splitCrossingSeg $ V.toList segsCross)


-- | Decide where to split the plane.
--   TODO: We're just taking the first point of the segment in the middle of the vector.
--      It might be better to base the split on:
--       - the closest segment
--       - the widest sgement
--       - the one closes to the middle of the field.
--       - some combination of above.
--
chooseSplitX :: V.Vector Segment -> Float
chooseSplitX segments
 = let  Just (_, (x1, _), _)    = segments V.!? (V.length segments `div` 2)
   in   x1



