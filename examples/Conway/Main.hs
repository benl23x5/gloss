
module Main where
import Config
import World
import Cell
import Graphics.Gloss

main 	
 = do	world	<- randomWorld
	simulateInWindow 
		"Conway" 
		(windowWidth, windowHeight)
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
	= Translate 
		(- fromIntegral windowWidth  / 2)
		(- fromIntegral windowHeight / 2)
	$ Pictures
	$ map (drawCell world) worldIndexes

drawCell :: World -> Index -> Picture
drawCell world index@(x, y)
 = let	cell	= getCell world index
	cs	= fromIntegral cellSize
	cp	= fromIntegral cellSpace
	fx	= fromIntegral x * (cs + cp) + 1
	fy	= fromIntegral y * (cs + cp) + 1
   in	Translate fx fy	$ pictureOfCell cell
		
