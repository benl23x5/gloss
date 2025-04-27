module Community where

import Cell
import Graphics.Gloss
import Data.List(zipWith4)

type Community = [Cell]

{- | does a (newly spawned) cell fit in the community?
that is, does it overlap with any others?
-}
fits :: Cell -> Community -> Bool
fits cell = not . any (overlap cell)

{- | For each member of a community, produce one offspring
The lists of Floats are the (random) parameters that determine size
and location of each offspring.
-}
spawn :: Community -> [Float] -> [Float] -> [Int] -> [Cell]
spawn = zipWith4 offspring

{- | Given a collection of cells (one spawned by each member of the
community) check if it fits, and if so add it to the community.
That check must include new cells that have been added to the
community in this process.
-}
survive :: Community -> [Cell] -> Community
survive =
  foldl (\comm c -> if fits c comm then c:comm else comm)

age :: Community -> Community
age = filter Cell.alive . map Cell.age

-- | The next generation of a community
generation :: Community ->  [Float] -> [Float] -> Community
generation comm angles scales =
    survive (Community.age comm) (spawn comm angles scales (repeat 5))

render :: Community -> Picture
render =
  Pictures . map Cell.render

initial :: Community
initial = [Cell (0,0) 50 5]
