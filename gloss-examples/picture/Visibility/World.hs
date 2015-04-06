
module World
        ( Segment
        , World(..)
        , initialWorld
        , normaliseWorld)
where
import Graphics.Gloss
import Geometry.Randomish
import Geometry.Segment
import qualified Data.Vector.Unboxed as V


-- We keep this unpacked so we can use unboxed vector.
-- index, x1, y1, x2, y2
data World 
        = World
        { worldSegments :: V.Vector Segment }


-- | Generate the initial world.
initialWorld :: IO World
initialWorld
 = do   let n           = 100
        let minZ        = -300
        let maxZ        = 300
        
        let minDelta    = -100
        let maxDelta    =  100
        
        let centers     = randomishPoints 1234 n minZ     maxZ
        let deltas      = randomishPoints 4321 n minDelta maxDelta

        let makePoint n' (cX, cY) (dX, dY)
                        = (n', (cX, cY), (cX + dX, cY + dY))

        let segs        = V.zipWith3 makePoint (V.enumFromTo 0 (n - 1)) centers deltas
        
        return $ World segs


-- | Normalise the world so that the given point is at the origin,
--   and split segements that cross the y=0 line.
normaliseWorld :: Point -> World -> World 
normaliseWorld (px, py) world
 = let  segments_trans  = V.map (translateSegment (-px) (-py)) 
                        $ worldSegments world
                        
        segments_split  = splitSegmentsOnY 0 segments_trans
                        
   in   world { worldSegments = segments_split }



