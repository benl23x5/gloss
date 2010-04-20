
module Cell where
import Graphics.Gloss	

-- | A cell in the world.
data Cell
	= -- | A living cell with its age
	  CellAlive Int	

	  -- | A dead / blank cell.
	| CellDead
	deriving (Show, Eq)


-- | Sort the living from the dead.
isAlive :: Cell -> Bool
isAlive cell
 = case cell of
	CellAlive _	-> True
	CellDead	-> False


-- | The basic shape of a cell.
cellShape :: Int -> Picture
cellShape cellSize
 = let 	cs	= fromIntegral cellSize
   in	Polygon [(0, 0), (cs, 0), (cs, cs), (0, cs)]


-- | Convert a cell to a picture, based on a primitive shape.
--	We pass the shape in to avoid recomputing it for each cell.
pictureOfCell :: Int -> Picture -> Cell -> Picture
pictureOfCell oldAge cellShape' cell
 = case cell of
	CellAlive age 	-> Color (ageColor oldAge age)	cellShape'
	CellDead  	-> Color (greyN 0.8) 	cellShape'

ageColor :: Int -> Int -> Color
ageColor oldAge age
 = let (r, g, b) = rampColorHotToCold 0 (fromIntegral oldAge) (fromIntegral age)
   in  makeColor r g b 1.0
	
	

-- Color Ramps  -----------------------------------------------------------------------------------
-- | Standard Hot -> Cold hypsometric color ramp.
--	Sequence is red, yellow, green, cyan, blue.
rampColorHotToCold 
	:: (Ord a, Floating a) 
	=> a 
	-> a 
	-> a 
	-> (a, a, a)
	
rampColorHotToCold vmin vmax vNotNorm
 = let	
	v	| vNotNorm < vmin	= vmin
	 	| vNotNorm > vmax	= vmax
		| otherwise		= vNotNorm
	
	dv	= vmax - vmin	

	result	| v < vmin + 0.25 * dv
		= ( 0
		  , 4 * (v - vmin) / dv
		  , 1.0)
		
		| v < vmin + 0.5 * dv
		= ( 0
		  , 1.0
		  , 1 + 4 * (vmin + 0.25 * dv - v) / dv)
		
		| v < vmin + 0.75 * dv
		= ( 4 * (v - vmin - 0.5 * dv) / dv
		  , 1.0
		  , 0.0)
		
		| otherwise
		= ( 1.0
		  , 1 + 4 * (vmin + 0.75 * dv - v) / dv
		  , 0)
		
  in	result

	