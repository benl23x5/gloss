
import Graphics.Gloss
import World
import Cell
import qualified Data.Vector	as V


main 
 = do	world		<- loadWorld "world.dat"
	let picture	= drawWorld world
	displayInWindow 
		"Occlusion"
		(windowSizeOfWorld world)
		(10, 10)
		black 
		picture
		



-- | Convert a world to a picture.
drawWorld
	:: World 
	-> Picture

drawWorld world	
 = let	(windowWidth, windowHeight)	
		= windowSizeOfWorld world
		
	offsetX	= - fromIntegral windowWidth  / 2
	offsetY	= - fromIntegral windowHeight / 2 
   in	Translate offsetX offsetY
		$ Pictures 
		$ V.toList 
		$ V.imap (drawCell world) (worldCells world)
		
		
-- | Convert a cell at a particular coordinate to a picture.
drawCell :: World -> Index -> Cell -> Picture
drawCell world index cell 
 = let	cs	= fromIntegral (worldCellSize world)
	cp	= fromIntegral (worldCellSpace world)

	(x, y)	= coordOfIndex world index
	fx	= fromIntegral x * (cs + cp) + 1
	fy	= fromIntegral y * (cs + cp) + 1

   in	pictureOfCell
		(worldCellSize   world)
		fx
		fy
		cell

-- | Get the size of the window needed to display a world.
windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world
 = let	cellSize	= worldCellSize world
	cellSpace	= worldCellSpace world
 	cellPad		= cellSize + cellSpace
 	height		= cellPad * (worldHeight world) + cellSpace
	width		= cellPad * (worldWidth  world) + cellSpace
   in	(width, height)
