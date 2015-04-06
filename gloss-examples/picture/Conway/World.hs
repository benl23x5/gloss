{-# LANGUAGE PatternGuards, ParallelListComp, BangPatterns #-}

module World where
import Cell
import System.Random
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import qualified Data.Vector    as Vec
type Vec        = Vec.Vector

-- Index ----------------------------------------------------------------------
-- | An index into the vector holding all the cells.
type Index      = Int

-- | The x y coordinate of a cell.
type Coord      = (Int, Int)

indexOfCoord :: World -> Coord -> Index
indexOfCoord world (x, y)       
        = x + y * (worldWidth world)

coordOfIndex :: World -> Index -> Coord
coordOfIndex world i            
        = ( i `mod` worldWidth world
          , i `div` worldWidth world)


-- World ----------------------------------------------------------------------
data World      
        = World
        { worldCells            :: Vec Cell 
        , worldWidth            :: Int 
        , worldHeight           :: Int 

        -- | Width and height of each cell.
        , worldCellSize         :: Int

        -- | Number of pixels to leave between each cell.
        , worldCellSpace        :: Int

        -- | Cells less than this age are drawn with the color ramp
        , worldCellOldAge       :: Int

        -- | Seconds to wait between each simulation step.
        , worldSimulationPeriod :: Float 
        
        -- | Time that has elapsed since we drew the last step
        , worldElapsedTime      :: Float }


-- | Make a new world of a particular size.
randomWorld :: (Int, Int) -> IO World
randomWorld (width, height)
 = do   bools   <- replicateM (width * height) randomIO 
        return  $ World
                { worldCells            = Vec.fromList $ map cellOfBool bools
                , worldWidth            = width
                , worldHeight           = height
                , worldCellSize         = 5
                , worldCellSpace        = 1 
                , worldCellOldAge       = 20
                , worldSimulationPeriod = 0.1 
                , worldElapsedTime      = 0 }


-- | Convert a bool to a live or dead cell.
cellOfBool :: Bool -> Cell
cellOfBool b
 = case b of
        True    -> CellAlive 0
        False   -> CellDead


-- | Get the cell at a particular coordinate in the world.
getCell :: World -> Coord -> Cell
getCell world coord@(x, y)
        | x < 0 || x >= worldWidth  world       = CellDead
        | y < 0 || y >= worldHeight world       = CellDead

        | otherwise             
        = worldCells world Vec.! indexOfCoord world coord 


-- | Get the neighbourhood of cells aroudn this coordinate.
getNeighbourhood :: World -> Coord -> [Cell]
getNeighbourhood world (ix, iy)
 = let  indexes = [ (x, y) 
                        | x <- [ix - 1 .. ix + 1]
                        , y <- [iy - 1 .. iy + 1]
                        , not (x == ix && y == iy) ]
   in   map (getCell world) indexes


-- | Compute the next cell state depending on its neighbours.
stepCell :: Cell -> [Cell] -> Cell
stepCell cell neighbours
 = let  live    = length (filter isAlive neighbours)
   in   case cell of
         CellAlive age  -> if elem live [2, 3] then CellAlive (age + 1) else CellDead
         CellDead       -> if live == 3        then CellAlive 0         else CellDead


-- | Compute the next state of the cell at this index in the world.
stepIndex :: World -> Int -> Cell -> Cell
stepIndex world index cell
 = let  coord   = coordOfIndex world index
        neigh   = getNeighbourhood world coord
   in   stepCell cell neigh

                
-- | Compute the next world state.
stepWorld :: World -> World
stepWorld world
        = world { worldCells = Vec.imap (stepIndex world) (worldCells world) }

                
-- | Simulation function for worlds.
simulateWorld :: ViewPort -> Float -> World -> World
simulateWorld _ time world 

        -- If enough time has passed then it's time to step the world.
        | worldElapsedTime world >= (worldSimulationPeriod world)
        = let world'    = stepWorld world
          in  world' { worldElapsedTime = 0 }
        
        -- Wait some more.
        | otherwise
        = world { worldElapsedTime = worldElapsedTime world + time }


