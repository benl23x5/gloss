
module Cell where
import Config
import Graphics.Gloss	

data Cell
	= CellAlive
	| CellDead
	deriving (Show, Eq)

isAlive cell
 = case cell of
	CellAlive	-> True
	CellDead	-> False

cellShape	
 = let 	cs	= fromIntegral cellSize
   in	Polygon [(0, 0), (cs, 0), (cs, cs), (0, cs)]

pictureOfCell :: Cell -> Picture
pictureOfCell cell
 = case cell of
	CellAlive -> Color black 	cellShape
	CellDead  -> Color (greyN 0.8) 	cellShape
