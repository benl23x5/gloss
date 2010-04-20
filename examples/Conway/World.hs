{-# LANGUAGE PatternGuards, ParallelListComp, BangPatterns #-}

module World where
import Cell
import System.Random
import Control.Monad
import Graphics.Gloss
import qualified Data.Vector	as Vec
type Vec	= Vec.Vector

-- Index ----------------------------------------------------------------------
type Index	= Int
type Coord	= (Int, Int)

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
	{ worldCells		:: Vec Cell 
	, worldWidth		:: Int 
	, worldHeight		:: Int 
	, worldLastTime		:: Float 
	, worldCellSize		:: Int
	, worldCellSpace	:: Int
	, worldSimulationPeriod	:: Float }


worldCoords :: World -> [Coord]
worldCoords world
 = [ (ix, iy)
	| ix	<- [0 .. worldWidth  world - 1]
	, iy	<- [0 .. worldHeight world - 1] ]


randomWorld :: (Int, Int) -> IO World
randomWorld (width, height)
 = do	bools	<- replicateM (width * height) randomIO 
	return	$ World
		{ worldCells		= Vec.fromList $ map cellOfBool bools
		, worldLastTime		= 0
		, worldWidth		= width
		, worldHeight		= height
		, worldCellSize		= 5
		, worldCellSpace	= 1 
		, worldSimulationPeriod	= 0.1 }


cellOfBool :: Bool -> Cell
cellOfBool b
 = case b of
	True	-> CellAlive
	False	-> CellDead


getCell :: World -> Coord -> Cell
getCell world coord@(x, y)
	| x < 0 || x >= worldWidth  world	= CellDead
	| y < 0 || y >= worldHeight world	= CellDead

	| otherwise		
	= worldCells world Vec.! indexOfCoord world coord 


getNeighbourhood :: World -> Coord -> [Cell]
getNeighbourhood world (ix, iy)
 = let	indexes	= [ (x, y) 
			| x <- [ix - 1 .. ix + 1]
			, y <- [iy - 1 .. iy + 1]
			, not (x == ix && y == iy) ]
   in	map (getCell world) indexes


stepCell :: Cell -> [Cell] -> Cell
stepCell cell neighbours
 = let 	live	= length (filter isAlive neighbours)
   in	case cell of
	 CellAlive	-> if elem live [2, 3] then CellAlive else CellDead
	 CellDead	-> if live == 3        then CellAlive else CellDead


stepIndex :: World -> Int -> Cell -> Cell
stepIndex world index cell
	= stepCell cell (getNeighbourhood world (coordOfIndex world index))

		
stepWorld :: World -> World
stepWorld world
	= world { worldCells	
			= Vec.imap (stepIndex world) (worldCells world) }

		
simulateWorld :: ViewPort -> Float -> World -> World
simulateWorld port time world 
	| worldLastTime world >= (worldSimulationPeriod world)
	= let world'	= stepWorld world
	  in  world' { worldLastTime = 0 }
	
	| otherwise
	= world { worldLastTime = worldLastTime world + time }

