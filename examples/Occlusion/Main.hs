{-# LANGUAGE PatternGuards #-}

import Graphics.Gloss
import World
import Cell
import QuadTree
import Extent
import Cast
import Data.Maybe
import Data.List
import Data.Function

main 
 = do	world		<- loadWorld "world.dat"
	let gameState	= initGameState world

	gameInWindow 
		"Occlusion"
		(windowSizeOfWorld world)
		(10, 10)
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
	, gameStateLineEnd	= (10, 10) }
	

handleInput :: World -> Event -> GameState -> GameState
handleInput world (EventKey key keyState mods pos) state
	| MouseButton LeftButton	<- key
	, Down				<- keyState
	, shift mods == Down	
	= state { gameStateLineEnd = worldPosOfWindowPos world pos }

	| MouseButton LeftButton	<- key
	, Down				<- keyState
	= state { gameStateLineStart 	= worldPosOfWindowPos world pos 
		, gameStateLineEnd	= worldPosOfWindowPos world pos }

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

	-- The ray
	p1		= gameStateLineStart state
	p2		= gameStateLineEnd   state
	ray		= (p1, p2)
	picRay		= drawRay world ray

	-- The cells
	cellsSeen	= [ (coord, cell)
				| (coord, cell)	<- flattenQuadTree (worldExtent world) (worldTree world)
				, canSeeCellAtCoord world p1 coord ]

	picCells	= Pictures 
			$ map (uncurry (drawCell world)) 
			$ cellsSeen

	-- The seen cell (if any)
	mSeenCell	= castRayIntoWorld world ray
	hotCells	= maybeToList mSeenCell
	picHot		= Pictures $ map (drawHot world) hotCells


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


canSeeCellAtCoord :: World -> Pos -> Coord -> Bool
canSeeCellAtCoord world posFrom coord@(x', y')
 = let	x	= fromIntegral x' + 0.5
	y	= fromIntegral y' + 0.5
	
	p1	= (x - 0.5, y)
	p2	= (x + 0.5, y)
	p3	= (x, y - 0.5)
	p4	= (x, y + 0.5)
	
   in	or $ map (canSeeCell world posFrom) [p1, p2, p3, p4]
	
canSeeCell world posFrom posTo
 = let	mOccluder	= castRayIntoWorld world (posFrom, posTo)	
   in	case mOccluder of
	 Nothing 			-> False
	 Just (pos, extent, cell)	-> posInExtent extent posTo


castRayIntoWorld :: World -> Ray -> Maybe (Pos, Extent, Cell)
castRayIntoWorld world ray@(p1, _)
 = let	hitCells	= cast ray (worldExtent world) (worldTree world)
	hitCells_sorted	= sortBy ((compareDistanceTo p1) `on` (\(a, b, c) -> a)) hitCells
	
   in	case hitCells_sorted of
	 (x:_)	-> Just x
	 _	-> Nothing
	
	

drawHot :: World -> (Pos, Extent, Cell) -> Picture
drawHot world (pos, extent, cell)
 = let	Extent n s e w	= extent
	x		= w
	y		= s

	posX	= fromIntegral x 
	posY	= fromIntegral y
	
   in	Color blue  $ cellShape 1 posX posY


drawRay :: World -> Ray -> Picture 
drawRay world ray@(p1@(x, y), p2)
 = Pictures
	[ Color red $ Line [p1, p2]
	, Color red 
		$ Translate x y 
		$ Pictures 
			[ Line [(-0.3, -0.3), (0.3,  0.3)]
			, Line [(-0.3,  0.3), (0.3, -0.3)] ] ]


		
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


		    
