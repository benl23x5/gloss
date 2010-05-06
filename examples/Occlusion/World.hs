{-# LANGUAGE ScopedTypeVariables #-}

module World where
import Cell
import Graphics.Gloss.Game
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Algorithms.RayCast
import System.IO
import Control.Monad

-- | The game world.
data World	
	= World
	{ worldWidth		:: Int
	, worldHeight		:: Int
	, worldTree		:: QuadTree Cell
	, worldCellSize		:: Int
	, worldCellSpace	:: Int }
	deriving Show


-- | Get the extent covering the entire world.
worldExtent :: World -> Extent
worldExtent world
	= makeExtent (worldWidth world) 0 (worldHeight world) 0


-- | Load a world from a file.	
loadWorld :: FilePath -> IO World
loadWorld fileName
 = do	h		<- openFile fileName ReadMode
	"WORLD"		<- hGetLine h
	[width, height]	<- liftM (map read . words) $ hGetLine h
	dat		<- hGetContents h
	let (h:dat')	= lines dat
	let rows	= take height $ dat'

	let cells	= concat $ map (readLine width) $ reverse rows
	let extent	= makeExtent height 0 width 0
	return World
		{ worldWidth		= width
		, worldHeight		= height
		, worldTree		= makeWorldTree extent cells
		, worldCellSize		= 20
		, worldCellSpace	= 0 }

readLine :: Int -> String -> [Cell]
readLine width (s:str)
	= map readCell
	$ take width str



-- | Get the size of the window needed to display a world.
windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world
 = let	cellSize	= worldCellSize world
	cellSpace	= worldCellSpace world
 	cellPad		= cellSize + cellSpace
 	height		= cellPad * (worldHeight world) + cellSpace
	width		= cellPad * (worldWidth  world) + cellSpace
   in	(width, height)


-- | Create the tree representing the world from a list of all its cells.
makeWorldTree :: Extent -> [Cell] -> QuadTree Cell
makeWorldTree extent cells
 = foldr insert' emptyTree nonEmptyPosCells
 where 
	insert' (pos, cell) tree
	 = case insertByCoord extent pos cell tree of
		Nothing		-> tree
		Just tree'	-> tree'
	
	(width, height)	
		= sizeOfExtent extent
	
	posCells	
		= zip	[(x, y) | y <- [0 .. height - 1]
				, x <- [0 .. width - 1]]
			cells
	
	nonEmptyPosCells 
	 	= filter (\x -> snd x /= CellEmpty) posCells



-- | Get the world position coresponding to a point in the window.
worldPosOfWindowPos :: World -> Point -> Point
worldPosOfWindowPos world (x, y)
 = let	(windowSizeX, windowSizeY)
		= windowSizeOfWorld world
		
	offsetX	= fromIntegral $ windowSizeX `div` 2
	offsetY	= fromIntegral $ windowSizeY `div` 2
	
	scale	= fromIntegral $ worldCellSize world
	
	x'	= (x + offsetX) / scale
	y'	= (y + offsetY) / scale

  in	(x', y')


-- | Check if a the cell at a given coordinate is visible from a point.
cellAtCoordIsVisible :: World -> Coord -> Coord -> Bool
cellAtCoordIsVisible world cFrom coord@(x', y')
 = or $ map (cellAtPointIsVisible world pFrom) [pa, pb, pc, pd]
 where
	(cx, cy)	= cFrom
	pFrom		= (fromIntegral cx + 0.5 , fromIntegral cy + 0.5)

 	x :: Float	= fromIntegral x' + 0.5
	y :: Float	= fromIntegral y' + 0.5
	pa	= (x - 0.49, y)
	pb	= (x + 0.49, y)
	pc	= (x, y - 0.49)
	pd	= (x, y + 0.49)
	
	
-- | Check if a point on some cell (P2) is visible from some other point (P1).
cellAtPointIsVisible :: World -> Point -> Point -> Bool
cellAtPointIsVisible world p1 p2
 = let	mOccluder	= castSegIntoWorld world p1 p2
   in	case mOccluder of
	 Nothing 			-> False
	 Just (pos, extent, cell)	-> pointInExtent extent p2


-- | Given a line segment (P1-P2) get the cell closest to P1 that intersects the segment.
castSegIntoWorld :: World -> Point -> Point -> Maybe (Point, Extent, Cell)
castSegIntoWorld world p1 p2
	= castSegIntoCellularQuadTree p1 p2 (worldExtent world) (worldTree world)


-- | Given a line segment (P1-P2) get the cell closest to P1 that intersects the segment.
traceSegIntoWorld :: World -> Point -> Point -> [(Point, Extent, Cell)]
traceSegIntoWorld world p1 p2
	= traceSegIntoCellularQuadTree p1 p2 (worldExtent world) (worldTree world)


