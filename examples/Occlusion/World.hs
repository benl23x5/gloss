
module World where

import Cell
import Data.Vector		(Vector)
import System.IO
import Control.Monad
import qualified Data.Vector	as V


-- Index ----------------------------------------------------------------------
-- | An index into the vector holding all the cells.
type Index	= Int

-- | The x y coordinate of a cell.
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
	{ worldWidth		:: Int
	, worldHeight		:: Int
	, worldCells		:: Vector Cell 
	, worldCellSize		:: Int
	, worldCellSpace	:: Int }
	deriving Show
	
loadWorld :: FilePath -> IO World
loadWorld fileName
 = do	h		<- openFile fileName ReadMode
	"WORLD"		<- hGetLine h
	[width, height]	<- liftM (map read . words) $ hGetLine h
	dat		<- hGetContents h
	let (h:dat')	= lines dat
	let rows	= take height $ dat'
	let cells	= concat $ map (readLine width) $ reverse rows
	return World
		{ worldWidth		= width
		, worldHeight		= height
		, worldCells		= V.fromList cells 
		, worldCellSize		= 20
		, worldCellSpace	= 1 }

readLine :: Int -> String -> [Cell]
readLine width (s:str)
	= map readCell
	$ take width str




