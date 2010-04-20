{-# LANGUAGE PatternGuards, ParallelListComp, BangPatterns #-}

module World where
import Config
import Cell
import System.Random
import Control.Monad
import Graphics.Gloss
import qualified Data.Map	as Map
import Data.Map			(Map)

type Coord	= (Int, Int)
data World 	
	= World
		{ worldCells	:: Map Coord Cell 
		, worldLastTime	:: Float }



worldCoords :: [Coord]
worldCoords
 = [ (ix, iy)
	| ix	<- [0 .. worldWidth - 1]
	, iy	<- [0 .. worldHeight - 1] ]


randomWorld :: IO World
randomWorld 
 = do	bools	<- replicateM (worldHeight * worldWidth) randomIO 
	return	$ startWorld bools


startWorld :: [Bool] -> World
startWorld bools
 	= World
 	{ worldCells	= Map.fromList 
				[(i, cellOfBool b) 
					| i <- worldCoords
					| b <- bools ] 
	, worldLastTime	= 0 }


cellOfBool :: Bool -> Cell
cellOfBool b
 = case b of
	True	-> CellAlive
	False	-> CellDead


getCell :: World -> Coord -> Cell
getCell world coord@(x, y)
	| x < 0 || x >= worldWidth	= CellDead
	| y < 0 || y >= worldHeight	= CellDead

	| otherwise		
	= case Map.lookup coord (worldCells world) of
		Nothing		-> error "cellOfWorld: out of range"
		Just cell	-> cell


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


stepIndex :: World -> Coord -> Cell -> Cell
stepIndex world index cell
	= stepCell cell (getNeighbourhood world index)

		
stepWorld :: World -> World
stepWorld world
	= world { worldCells	= Map.mapWithKey (stepIndex world) (worldCells world) }

		
simulateWorld :: ViewPort -> Float -> World -> World
simulateWorld port time world 
	| worldLastTime world >= simPeriod
	= let world'	= stepWorld world
	  in  world' { worldLastTime = 0 }
	
	| otherwise
	= world { worldLastTime = worldLastTime world + time }

