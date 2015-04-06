
module Config where
import Graphics.Gloss

-- Number of simulation steps per second of time.
simResolution :: Int
simResolution           = 300

-- How strongly the beads are pulled down to the bottom of the screen.
--      If this is too high wrt the simResoution then the simulation
--      will be unstable and beads will escape the box.
gravityCoeff :: Float
gravityCoeff            = 300

-- Whether to draw velocity vectors on beads.
showBeadVelocity        = False

-- Colors of things.
beadColor               = makeColor 0.5 0.5 1.0 1.0
beadOutlineColor        = makeColor 1.0 1.0 1.0 1.0
nodeColor               = makeColor 0.2 0.8 0.2 0.1
leafColor               = makeColor 0.8 0.2 0.2 0.1

-- The maximum depth of the quad tree.
treeMaxDepth :: Int
treeMaxDepth            = 4

-- Size of quadtree. Should be > boxSize.
treeSize :: Float
treeSize                = 300

-- Size of bead box.
boxSize :: Float
boxSize                 = 280

-- Bead setup.
beadRadius, beadSpace, beadCountX, beadCountY, beadBoxSize :: Float

beadRadius              = 5
beadSpace               = 1
beadBoxSize             = 2 * beadRadius + beadSpace
beadCountX              = 20
beadCountY              = 10

beadStuckCount :: Int
beadStuckCount          = 20
