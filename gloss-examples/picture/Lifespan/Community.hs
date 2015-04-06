module Community where

import Cell
import Graphics.Gloss

type Community = [Cell]

-- does a (newly spawned) cell fit in the community?
-- that is, does it overlap with any others?
fits :: Cell -> Community -> Bool
fits cell cells = not $ any (overlap cell) cells

-- For each member of a community, produce one offspring
-- The lists of Floats are the (random) parameters that determine size

-- and location of each offspring.
spawn :: Community -> [Float] -> [Float] -> [Int] -> [Cell]
spawn = zipWith4 offspring

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f [] _ _ _ = []
zipWith4 f _ [] _ _ = []
zipWith4 f _ _ [] _ = []
zipWith4 f _ _ _ [] = []
zipWith4 f (b:bs) (c:cs) (d:ds) (e:es) =
    f b c d e : zipWith4 f bs cs ds es


-- Given a collection of cells (one spawned by each member of the
-- community) check if it fits, and if so add it to the community.
-- That check must include new cells that have been added to the
-- community in this process.
survive :: [Cell] -> Community -> Community
survive [] comm = comm
survive (cell:cells) comm
    | fits cell comm  = survive cells (cell:comm)
    | otherwise       = survive cells comm

age :: Community -> Community
age [] = []
age (Cell c r 0 : cells) = age cells
age (Cell c r life : cells) = Cell c r (life-1) : age cells


-- The next generation of a community
generation :: Community ->  [Float] -> [Float] -> Community
generation comm angles scales =
    survive (spawn comm angles scales (repeat 5)) (age comm)

render :: Community -> Picture
render comm 
        = Pictures 
        $ map Cell.render comm

initial :: Community
initial = [Cell (0,0) 50 5]


