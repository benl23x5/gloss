
import Graphics.Gloss
import World
import Cell
import QuadTree
import Extent
import Cast

main 
 = do	world		<- loadWorld "world.dat"
--	let picture	= drawWorld world
	let picture	= drawIntersectsHorzSeg ((10, 10), (20, 50)) 40 10 20

	displayInWindow 
		"Occlusion"
		(windowSizeOfWorld world)
		(1200, 10)
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
		$ map (uncurry (drawCell world)) 
		$ flattenQuadTree (worldExtent world) (worldTree world)
		
		
-- | Convert a cell at a particular coordinate to a picture.
drawCell :: World -> Coord -> Cell -> Picture
drawCell world (x, y) cell 
 = let	cs	= fromIntegral (worldCellSize world)
	cp	= fromIntegral (worldCellSpace world)

	fx	= fromIntegral x * (cs + cp) + 1
	fy	= fromIntegral y * (cs + cp) + 1

   in	pictureOfCell
		(worldCellSize world)
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



drawIntersectsHorzSeg :: Ray -> Float -> Float -> Float -> Picture
drawIntersectsHorzSeg ray@(p1, p2) y0 xa xb
 = let color	= if rayIntersectsHorzSeg ray y0 xa xb 
			then red
			else blue
   in  Scale 5 5
	$ Pictures
		[ Color color $ Line [p1, p2]
		, Color white $ Line [(xa, y0), (xb, y0)] ]
