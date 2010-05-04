
module World where

import Cell
import QuadTree
import Extent
import System.IO
import Control.Monad


data World	
	= World
	{ worldWidth		:: Int
	, worldHeight		:: Int
	, worldTree		:: QuadTree Cell
	, worldCellSize		:: Int
	, worldCellSpace	:: Int }
	deriving Show

worldExtent :: World -> Extent
worldExtent world
	= Extent (worldWidth world) 0 (worldHeight world) 0
	
loadWorld :: FilePath -> IO World
loadWorld fileName
 = do	h		<- openFile fileName ReadMode
	"WORLD"		<- hGetLine h
	[width, height]	<- liftM (map read . words) $ hGetLine h
	dat		<- hGetContents h
	let (h:dat')	= lines dat
	let rows	= take height $ dat'

	let cells	= concat $ map (readLine width) $ reverse rows
	let extent	= Extent width 0 height 0
	return World
		{ worldWidth		= width
		, worldHeight		= height
		, worldTree		= makeWorldTree extent cells
		, worldCellSize		= 20
		, worldCellSpace	= 1 }


readLine :: Int -> String -> [Cell]
readLine width (s:str)
	= map readCell
	$ take width str


makeWorldTree :: Extent -> [Cell] -> QuadTree Cell
makeWorldTree extent cells
 = foldr insert' emptyTree nonEmptyPosCells
 where 
	insert' (pos, cell) tree
	 = case insertNodeByCoord extent pos cell tree of
		Nothing		-> tree
		Just tree'	-> tree'
	
	(width, height)	
		= sizeOfExtent extent
	
	posCells	
		= zip	[(x, y) | x <- [0 .. width - 1]
				, y <- [0 .. height - 1]]
			cells
	
	nonEmptyPosCells 
	 	= filter (\x -> snd x /= CellEmpty) posCells


