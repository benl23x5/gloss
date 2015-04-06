module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random
import Community
import Cell

stepsMax        = 20

-- The World consists of a Community and a random number generator.
-- (The RNG is a model of chaos or hand-of-god.)
data World 
        = World Community StdGen Int
        deriving (Show)

-- The initial world
genesis :: World
genesis 
        = World [Cell (0,0) 50 5] (mkStdGen 1023) 0

-- Seeding the prng means every run is identical.
-- To get different runs, need to use gen <- getStdGen in main :: IO()
-- and pass gen in as an argument.  Edit Main.hs accordingly.
genesis' :: StdGen -> World
genesis' gen 
        = World [Cell (0,0) 50 5] gen 0

-- Consume some random numbers to advance the simulation
evolve :: ViewPort -> Float -> World -> World
evolve _ _ world@(World comm gen step) 
        | step > stepsMax       = world
        | otherwise
        = World (generation comm angles scales) genNext (step + 1)
        where   (genThis, genNext) = split gen
                (genA, genS)       = split genThis
                angles             = randomRs (0.0, 2*pi) genA
                scales             = randomRs (0.7, 0.9) genS

-- Converting the world to a picture is just converting the community component
render :: World -> Picture
render (World comm gen _) 
        = Color (makeColor 0.3 0.3 0.6 1.0)
        $ Community.render comm
