
module Cell where
import Graphics.Gloss	

data Cell
	= CellAlive
	| CellDead
	deriving (Show, Eq)

isAlive :: Cell -> Bool
isAlive cell
 = case cell of
	CellAlive	-> True
	CellDead	-> False

cellShape :: Int -> Picture
cellShape cellSize
 = let 	cs	= fromIntegral cellSize
   in	Polygon [(0, 0), (cs, 0), (cs, cs), (0, cs)]

pictureOfCell :: Picture -> Cell -> Picture
pictureOfCell cellShape' cell
 = case cell of
	CellAlive -> Color black 	cellShape'
	CellDead  -> Color (greyN 0.8) 	cellShape'
