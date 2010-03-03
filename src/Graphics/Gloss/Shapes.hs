
-- | Helpers for drawing common shapes.
module Graphics.Gloss.Shapes
	( lineLoop
	, rectangleWire, 	rectangleSolid, 	rectanglePath
	, rectangleUpperWire, 	rectangleUpperSolid,	rectangleUpperPath )
where
import Graphics.Gloss.Picture

-- | Closes a path by adding an additional point to the end, which equals the point at the start.
lineLoop :: Path -> Picture
lineLoop []	= Line []
lineLoop (x:xs)	= Line ((x:xs) ++ [x])


-- Rectangles -------------------------------------------------------------------------------------
-- | A wireframe rectangle centered about the origin.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY
	= lineLoop $ rectanglePath sizeX sizeY


-- | A solid rectangle centered about the origin.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY
	= Polygon $ rectanglePath sizeX sizeY


-- | A path representing a rectangle centered about the origin.
rectanglePath :: Float -> Float -> Path
rectanglePath sizeX sizeY			
 = let	sx	= sizeX / 2
	sy	= sizeY / 2
   in	[(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY
	= lineLoop $ rectangleUpperPath sizeX sizeY


-- | A sold rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY
	= Polygon  $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy
 = let 	sx	= sizeX / 2
   in  	[(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]

