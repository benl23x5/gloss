
module Cell where
import Graphics.Gloss	

-- | A cell in the world.
data Cell
	= CellAlive
	| CellDead
	deriving (Show, Eq)


-- | Sort the living from the dead.
isAlive :: Cell -> Bool
isAlive cell
 = case cell of
	CellAlive	-> True
	CellDead	-> False


-- | The basic shape of a cell.
cellShape :: Int -> Picture
cellShape cellSize
 = let 	cs	= fromIntegral cellSize
   in	Polygon [(0, 0), (cs, 0), (cs, cs), (0, cs)]


-- | Convert a cell to a picture, based on a primitive shape.
--	We pass the shape in to avoid recomputing it for each cell.
pictureOfCell :: Picture -> Cell -> Picture
pictureOfCell cellShape' cell
 = case cell of
	CellAlive -> Color black 	cellShape'
	CellDead  -> Color (greyN 0.8) 	cellShape'
