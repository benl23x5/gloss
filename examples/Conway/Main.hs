
module Main where
import World
import Cell
import Graphics.Gloss

main 	
 = do	
	let width	= 150
	let height	= 100
	world	<- randomWorld (width, height)
	
	simulateInWindow 
		"Conway" 
		(windowSizeOfWorld world)
		(5, 5) 
		white
		10 
		world
		drawWorld
		simulateWorld
	

-- | Draw a world as a picture.
drawWorld
	:: World 
	-> Picture

drawWorld world	
 = let	(windowWidth, windowHeight)	
		= windowSizeOfWorld world
		
	offsetX	= - fromIntegral windowWidth  / 2
	offsetY	= - fromIntegral windowHeight / 2 
   in	Translate offsetX offsetY
		$ Pictures $ map (drawCell world) (worldCoords world)

drawCell :: World -> Coord -> Picture
drawCell world coord@(x, y)
 = let	cell	= getCell world coord
	cs	= fromIntegral (worldCellSize world)
	cp	= fromIntegral (worldCellSpace world)
	fx	= fromIntegral x * (cs + cp) + 1
	fy	= fromIntegral y * (cs + cp) + 1
	shape	= cellShape (worldCellSize world)	
   in	Translate fx fy	$ pictureOfCell shape cell
		

windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world
 = let	cellSize	= worldCellSize world
	cellSpace	= worldCellSpace world
 	cellPad		= cellSize + cellSpace
 	height		= cellPad * (worldHeight world) + cellSpace
	width		= cellPad * (worldWidth  world) + cellSpace
   in	(width, height)

