module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random
import Community

stepsMax :: Int
stepsMax = 20

{- | The World consists of a Community and a random number generator.
(The RNG is a model of chaos or hand-of-god.)
-}
data World = World Community StdGen Int
        deriving (Show)

genesis :: StdGen -> World
genesis gen = World Community.initial gen 0

-- | Consume some random numbers to advance the simulation
evolve :: ViewPort -> Float -> World -> World
evolve _ _ world@(World comm gen step)
  | step > stepsMax = world
  | otherwise = World (generation comm angles scales) genNext (step + 1)
    where (genThis, genNext) = split gen
          (genA, genS)       = split genThis
          angles             = randomRs (0.0, 2*pi) genA
          scales             = randomRs (0.7, 0.9) genS

render :: World -> Picture
render (World comm _ _) =
  Color (makeColor 0.3 0.3 0.6 1.0) $ Community.render comm
