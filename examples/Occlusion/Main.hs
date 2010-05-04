{-# LANGUAGE PatternGuards #-}

import Graphics.Gloss
import World
import Cell
import QuadTree
import Extent
import Cast
import Data.Maybe

main 
 = do	world		<- loadWorld "world.dat"
	let gameState	= initGameState world

	gameInWindow 
		"Occlusion"
		(windowSizeOfWorld world)
		(1200, 10)
		black 
		100 
		gameState
		drawState
		(handleInput world)
		(\_ -> id)
				

data GameState
	= GameState
	{ gameStateWorld	:: World
	, gameStateLineStart	:: Pos
	, gameStateLineEnd	:: Pos }


initGameState world
	= GameState
	{ gameStateWorld	= world
	, gameStateLineStart	= (10, 10)
	, gameStateLineEnd	= (100, 100) }
	

handleInput :: World -> Event -> GameState -> GameState
handleInput world (EventKey key keyState mods pos) state
	| MouseButton LeftButton	<- key
	, Down				<- keyState
	= state { gameStateLineStart = worldPosOfWindowPos world pos }

	| MouseButton RightButton	<- key
	, Down				<- keyState
	= state { gameStateLineEnd = worldPosOfWindowPos world pos }
	
handleInput _ _ state
	= state

worldPosOfWindowPos :: World -> Pos -> Pos
worldPosOfWindowPos world (x, y)
 = let	(windowSizeX, windowSizeY)
		= windowSizeOfWorld world
		
	offsetX	= fromIntegral $ windowSizeX `div` 2
	offsetY	= fromIntegral $ windowSizeY `div` 2
	
	scale	= fromIntegral $ worldCellSize world
	
	x'	= (x + offsetX) / scale
	y'	= (y + offsetY) / scale

  in	(x', y')


-- | Convert the state to a picture.
drawState :: GameState -> Picture
drawState state
 = let	
	world		= gameStateWorld state

	-- The cells
	picCells	= Pictures 
			$ map (uncurry (drawCell world)) 
			$ flattenQuadTree (worldExtent world) (worldTree world)

	-- The hot cells
	hotCells	= cast ray (worldExtent world) (worldTree world)
	picHot		= Pictures $ map (drawHot world) hotCells

	-- The ray
	p1		= gameStateLineStart state
	p2		= gameStateLineEnd   state
	ray		= (p1, p2)
	picRay		= drawRay world ray

	-- Scale the world so it fits nicely in the window.
	scale		= fromIntegral $ worldCellSize world

	(windowSizeX, windowSizeY)	
		= windowSizeOfWorld
		$ gameStateWorld state
		
	offsetX	= - (fromIntegral $ windowSizeX `div` 2)
	offsetY	= - (fromIntegral $ windowSizeY `div` 2)

   in	Translate offsetX offsetY
		$ Scale scale scale
		$ Pictures [ picCells, picHot, picRay ]


drawHot :: World -> (Pos, Extent, Cell) -> Picture
drawHot world (pos@(ix, iy), extent, cell)
 = let	Extent n s e w	= extent
	x		= w
	y		= s

	posX	= fromIntegral x 
	posY	= fromIntegral y
	
   in	Pictures
		[ Color red   $ cellShape 1 posX posY ]


drawRay :: World -> Ray -> Picture 
drawRay world ray@(p1, p2)
 	= Color red $ Line [p1, p2]


		
-- | Convert a cell at a particular coordinate to a picture.
drawCell :: World -> Coord -> Cell -> Picture
drawCell world (x, y) cell 
 = let	
	cs	= fromIntegral (worldCellSize world)
	cp	= fromIntegral (worldCellSpace world)

	posX	= fromIntegral x 
	posY	= fromIntegral y

   in	pictureOfCell
		(worldCellSize world)
		posX
		posY
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


		    
