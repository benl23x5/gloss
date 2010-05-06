{-# LANGUAGE PatternGuards #-}

import World
import State
import Cell
import Graphics.Gloss.Game
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Shapes
import System.Environment
import Data.Maybe
import Data.List
import Data.Function

main 
 = do	[fileName]	<- getArgs
	world		<- loadWorld fileName
	let gameState	= initState world
	print $ windowSizeOfWorld world
	gameInWindow 
		"Occlusion"
		(windowSizeOfWorld world)
		(10, 10)
		black 
		10
		gameState
		drawState
		(handleInput world)
		(\_ -> id)
				
-- | Convert the state to a picture.
drawState :: State -> Picture
drawState state
 = let	
	world		= stateWorld state

	-- The ray
	p1@(xDude, yDude) = stateLineStart state
	p2		  = stateLineEnd   state
	picRay		  = drawRay world p1 p2

	-- The cells
	cDude		= (truncate xDude, truncate yDude)
	cellsAll	= flattenQuadTree (worldExtent world) (worldTree world)
	picCellsAll	= Pictures $ map (uncurry (drawCell False world)) cellsAll

	cellsSeen	= [ (coord, cell)
				| (coord, cell)	<- flattenQuadTree (worldExtent world) (worldTree world)
				, cellAtCoordIsVisible world cDude coord ]

	picCellsSeen	= Pictures $ map (uncurry (drawCell True world)) cellsSeen

	-- The seen cell (if any)
--	mSeenCell	= castSegIntoWorld world p1 p2
--	hotCells	= maybeToList mSeenCell
	hotCells	= traceSegIntoWorld world p1 p2
	picHot		= Pictures $ map (drawHot world) hotCells

	-- Scale the world so it fits nicely in the window.
	scale		= fromIntegral $ worldCellSize world

	(windowSizeX, windowSizeY)	
		= windowSizeOfWorld
		$ stateWorld state
		
	offsetX	= - (fromIntegral $ windowSizeX `div` 2)
	offsetY	= - (fromIntegral $ windowSizeY `div` 2)

   in	Translate offsetX offsetY
		$ Scale scale scale
		$ Pictures [ picCellsAll, picCellsSeen, picHot, picRay ]


	
drawHot :: World -> (Point, Extent, Cell) -> Picture
drawHot world (pos@(px, py), extent, cell)
 = let	(n, s, e, w)	= takeExtent extent
	x		= w
	y		= s

	posX	= fromIntegral x 
	posY	= fromIntegral y
	
   in	Pictures 
	 [ Color blue 	$ cellShape 1 posX posY
	 , Color green  
		$ Translate px py 
		$ Pictures 
			[ Line [(-0.2, -0.2), (0.2,  0.2)]
			, Line [(-0.2,  0.2), (0.2, -0.2)]]]


drawRay :: World -> Point -> Point -> Picture 
drawRay world p1@(x, y) p2
 = Pictures
	[ Color red $ Line [p1, p2]
	, Color cyan 
		$ Translate x y 
		$ Pictures 
			[ Line [(-0.3, -0.3), (0.3,  0.3)]
			, Line [(-0.3,  0.3), (0.3, -0.3)] ] ]


		
-- | Convert a cell at a particular coordinate to a picture.
drawCell :: Bool -> World -> Coord -> Cell -> Picture
drawCell seen world (x, y) cell 
 = let	
	cs	= fromIntegral (worldCellSize world)
	cp	= fromIntegral (worldCellSpace world)

	posX	= fromIntegral x 
	posY	= fromIntegral y

   in	case seen of
	 True	-> pictureOfCell
			(worldCellSize world)
			posX
			posY
			cell

	 False	-> case cell of
			CellEmpty -> Color (greyN 0.4)	(cellShape cs posX posY)
			CellWall  -> Color (greyN 0.4)	(cellShape cs posX posY)



		    
