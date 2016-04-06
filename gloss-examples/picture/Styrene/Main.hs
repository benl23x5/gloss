
import Actor
import Advance
import QuadTree
import Contact
import Collide
import World
import Config

import Graphics.Gloss
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Vector

import qualified Data.Map       as Map
import Data.Map                 (Map)

main 
  = simulate 
        (InWindow  "Polystyrene - alt-left-click-drag rotates"
                   (600, 600)   -- x and y size of window (in pixels).
                   (10, 10))    -- position of window
        black                   -- background color
        simResolution           -- simulation resolution  
                                --    (number of steps to take for each second of time)
        worldInit               -- the initial world.
        drawWorld               -- a function to convert the world to a Picture.
        advanceWorld            -- a function to advance the world to
                                --    the next simulation step.

-- Draw -------------------------------------------------------------------------------------------

-- | Draw this world as a picture.
drawWorld :: World -> Picture
drawWorld (World actors tree)
 = let 
        -- split the list of actors into beads and walls.
        --      this lets us draw all the beads at once without having to keep changing 
        --      the current color (which is a bit of a performance improvement)
        (beads, walls)  = splitActors $ Map.elems actors
   
        picBeads        = Color beadColor $ Pictures $ map drawActor beads
        picWalls        = Pictures $ map drawActor walls
        picTree         = drawQuadTree tree

   in   Scale 0.8 0.8
        $ Pictures [picTree, picWalls, picBeads]


-- | Split actors into beads and walls
splitActors :: [Actor] -> ([Actor], [Actor])
splitActors as
        = splitActors' [] [] as

splitActors' accBeads accWalls []               
        = (accBeads, accWalls)

splitActors' accBeads accWalls (a : as)         
 = case a of
        Bead{}  -> splitActors' (a : accBeads) accWalls as
        Wall{}  -> splitActors' accBeads (a : accWalls) as


-- | Draw an actor as a picture.
drawActor :: Actor -> Picture 
drawActor actor 
 = case actor of
        Bead ix mode radius p@(posX, posY) v@(velX, velY)
         -> Translate posX posY $ Pictures [bead, vel]
         where  bead    = circleFilled radius 10
                vel     = if showBeadVelocity
                                then Color red $ Line [(0, 0), mulSV 0.1 v]
                                else Blank
        Wall _ p1 p2
                -> Color (greyN 0.8) $ Line [p1, p2]


-- | Draw a quadtree as a picture
drawQuadTree :: QuadTree a -> Picture
drawQuadTree tree 
 = case tree of
        QNode p size tTL tTR tBL tBR
         ->  Pictures
                [ drawQuadTree tTL 
                , drawQuadTree tTR
                , drawQuadTree tBL
                , drawQuadTree tBR
                , nodeBox p size nodeColor ]

        QLeaf p size elems
         -> nodeBox p size leafColor
         
        QNil (x0, y0) size
         -> Blank

nodeBox p@(x0, y0) size color
        = Color color
        $ Translate x0 y0
        $ rectangleWire (size*2) (size*2)


-- Make a circle of radius r consisting of n lines.
circleFilled :: Float -> Float -> Picture
circleFilled r n
        = Scale r r
        $ Polygon (circlePoints n)
        
        
-- A list of n points spaced equally around the unit circle.
circlePoints :: Float -> [(Float, Float)]
circlePoints n
 =      map     (\d -> (cos d, sin d))
                [0, 2*pi / n .. 2*pi]
