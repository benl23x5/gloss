
-- | Functions for drawing shapes that aren't constructors of the `Picture` data type.
module Graphics.Gloss.Shapes
	( lineLoop
	, rectangleWire, 	rectangleSolid, 	rectanglePath
	, rectangleUpperWire, 	rectangleUpperSolid,	rectangleUpperPath )
where
import Graphics.Gloss.Data.Picture

-- | A closed loop along this path.
lineLoop :: Path -> Picture
lineLoop []	= Line []
lineLoop (x:xs)	= Line ((x:xs) ++ [x])


-- Rectangles -------------------------------------------------------------------------------------
-- | A wireframe rectangle centered about the origin.
rectangleWire 
	:: Float 	-- ^ width
	-> Float 	-- ^ height
	-> Picture
rectangleWire sizeX sizeY
	= lineLoop $ rectanglePath sizeX sizeY


-- | A solid rectangle centered about the origin.
rectangleSolid 
	:: Float 	-- ^ width
	-> Float 	-- ^ height
	-> Picture

rectangleSolid sizeX sizeY
	= Polygon $ rectanglePath sizeX sizeY


-- | A path representing a rectangle centered about the origin.
rectanglePath 
	:: Float 	-- ^ width
	-> Float 	-- ^ height
	-> Path

rectanglePath sizeX sizeY			
 = let	sx	= sizeX / 2
	sy	= sizeY / 2
   in	[(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire 
	:: Float 	-- ^ width
	-> Float 	-- ^ height
	-> Picture

rectangleUpperWire sizeX sizeY
	= lineLoop $ rectangleUpperPath sizeX sizeY


-- | A sold rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid 
	:: Float 	-- ^ width
	-> Float 	-- ^ height
	-> Picture

rectangleUpperSolid sizeX sizeY
	= Polygon  $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath 
	:: Float 	-- ^ width
	-> Float 	-- ^ height
	-> Path

rectangleUpperPath sizeX sy
 = let 	sx	= sizeX / 2
   in  	[(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]

