module Community where

import Cell
import Graphics.Gloss

type Community = [Cell]

-- does a (newly spawned) cell fit in the community?
-- that is, does it overlap with any others?
fits :: Cell -> Community -> Bool
fits cell cells 
        = not $ any (overlap cell) cells

-- For each member of a community, produce one offspring
-- The lists of Floats are the (random) parameters that determine size
-- and location of each offspring.
spawn :: Community -> [Float] -> [Float] -> [Cell]
spawn   = zipWith3 offspring

-- Given a collection of cells (one spawned by each member of the
-- community) check if it fits, and if so add it to the community.
-- That check must include new cells that have been added to the
-- community in this process.
survive :: [Cell] -> Community -> Community
survive [] comm = comm
survive (cell:cells) comm
        | fits cell comm  = survive cells (cell:comm)
        | otherwise       = survive cells comm

-- The next generation of a community
generation :: Community ->  [Float] -> [Float] -> Community
generation comm angles scales 
        = survive (spawn comm angles scales) comm

render :: Community -> Picture
render comm 
        = Pictures $ map Cell.render comm

initial :: Community
initial = [Cell (0,0) 50 0]


-- thread the random lists for testing outside IO()
--
life :: Community -> [Float] -> [Float] -> (Community, [Float], [Float])
life comm randomAngles randomScales =
    (generation comm angles scales, randomAngles', randomScales')
    where population = length comm
          (angles, randomAngles') = splitAt population randomAngles
          (scales, randomScales') = splitAt population randomScales

evolution :: Community -> [Float] -> [Float] -> [Community]
evolution comm randomAngles randomScales = comm1 : comms
    where (comm1, ras, rss) = life comm randomAngles randomScales
          comms = evolution comm1 ras rss
