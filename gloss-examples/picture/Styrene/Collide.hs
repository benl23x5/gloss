-- | Physics for bead bouncing.
module Collide where
import World
import Actor
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Geometry.Angle

-- Config -----------------------------------------------------------------------------------------
-- How bouncy the beads are
--      at 0.2 and they look like melting plastic.
--      at 0.8 and they look like bouncy rubber balls.
--      at > 1 and they gain energy with each bounce and escape the box.
--
beadBeadLoss    = 0.95
beadWallLoss    = 0.8


-- | Move a bead which is in contact with a wall.
collideBeadWall
        :: Actor        -- ^ the bead 
        -> Actor        -- ^ the wall that bead is in contact with
        -> Actor        -- ^ the new bead

collideBeadWall
        bead@(Bead ix _ radius pBead vIn@(velX, velY))
        wall@(Wall _ pWall1 pWall2)

 = let  -- Take the collision point as being the point on the wall which is 
        -- closest to the bead's center.
        pCollision      = closestPointOnLine pWall1 pWall2 pBead
 
        -- then do a static, non energy transfering collision.
  in    collideBeadPoint_static 
                bead 
                pCollision
                beadWallLoss


-- | Move two beads which have bounced into each other.
collideBeadBead_elastic
        :: Actor -> Actor
        -> (Actor, Actor)

collideBeadBead_elastic
        bead1@(Bead ix1 mode1 r1 p1 v1) 
        bead2@(Bead ix2 mode2 r2 p2 v2)

 = let  mass1   = 1
        mass2   = 1

        -- the axis of collision (towards p2)
        vCollision@(cX, cY)     = normalizeV (p2 - p1)
        vCollisionR             = (cY, -cX)
        
        -- the velocity component of each bead along the axis of collision
        s1      = dotV v1 vCollision
        s2      = dotV v2 vCollision

        -- work out new velocities along the collision
        s1'     = (s1 * (mass1 - mass2) + 2 * mass2 * s2) / (mass1 + mass2)
        s2'     = (s2 * (mass2 - mass1) + 2 * mass1 * s1) / (mass1 + mass2)
        
        -- the velocity components at right angles to the collision
        --      there is no friction in the collision so these don't change
        k1      = dotV v1 vCollisionR
        k2      = dotV v2 vCollisionR
        
        -- new bead velocities
        v1'     = mulSV s1' vCollision + mulSV k1 vCollisionR
        v2'     = mulSV s2' vCollision + mulSV k2 vCollisionR

        v1_slow = mulSV beadBeadLoss v1'
        v2_slow = mulSV beadBeadLoss v2'

        -- work out the point of collision
        u1      = r1 / (r1 + r2)
        u2      = r2 / (r1 + r2)

        pCollision      
                = p1 + mulSV u1 (p2 - p1)

        -- place the beads just next to each other so they are no longer overlapping.
        p1'     = pCollision - (r1 + 0.001) `mulSV` vCollision
        p2'     = pCollision + (r2 + 0.001) `mulSV` vCollision

        bead1'  = Bead ix1 mode1 r1 p1' v1_slow
        bead2'  = Bead ix2 mode2 r2 p2' v2_slow

   in   (bead1', bead2')


collideBeadBead_static
        :: Actor -> Actor 
        -> Actor
        
collideBeadBead_static
        bead1@(Bead ix1 _ radius1 pBead1 _)
        bead2@(Bead ix2 _ radius2 pBead2 _)

 = let  -- Take the collision point as being between the center's of the two beads. 
        -- For beads which have the same radius the collision point is half way between
        -- their centers and u == 0.5
        u               = radius1 / (radius1 + radius2)
        pCollision      = pBead1 + mulSV u (pBead2 - pBead1)
                
        bead1'          = collideBeadPoint_static
                                bead1
                                pCollision
                                beadBeadLoss
   in   bead1'


-- | Move a bead which has collided with something.
collideBeadPoint_static
        :: Actor -- ^ the bead which collided with something
        -> Point -- ^ the point of collision (should be near the bead's surface)
        -> Float -- ^ velocity scaling factor (how much to slow the bead down after the collision)
        -> Actor

collideBeadPoint_static
        bead@(Bead ix mode radius pBead vIn)    
        pCollision
        velLoss
 = let
        -- take a normal vector from the wall to the bead.
        --      this vector is at a right angle to the wall.
        vNormal         = normalizeV (pBead - pCollision)
        
        -- the bead at pBead is overlapping with what it collided with, but we don't want that.
        --      place the bead so it's surface is just next to the point of collision.
        pBead_new       = pCollision + (radius + 0.01) `mulSV` vNormal

        -- work out the angle of incidence for the bounce.
        --      this is the angle between the surface normal and
        --      the direction of travel for the bead.
        aInc            = angleVV vNormal (negate vIn)

        -- aInc2 is the angle between the wall /surface/ and
        --      the direction of travel.
        aInc2           = (pi / 2) - aInc

        -- take the determinant between the surface normal and the direction of travel.
        --      This will tell us what direction the bead hit the wall. 
        --      The diagram shows the sign of the determinant for the four possiblities.
        --
        --           \ +ve                                -ve /
        --            \                                      /
        --             \/                                  \/
        --   pWall1 ---------- pWall2           pWall1 ---------- pWall2
        --             /\                                  /\
        --            /                                      \
        --           / -ve                                +ve \
        --
        determinant     = detV vIn vNormal

        -- Use the determinant to rotate the bead's velocity vector for the bounce.
        vOut            
         | determinant > 0      = rotateV (2 * aInc2) vIn
         | otherwise            = rotateV (negate (2 * aInc2)) vIn

        -- Slow down the bead when it hits the wall
        vSlow           = velLoss `mulSV` vOut

        bead1_new       = Bead ix mode radius pBead_new vSlow

   in   bead1_new
