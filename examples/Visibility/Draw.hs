{-# LANGUAGE PatternGuards #-}
module Draw
	( drawState
	, drawWorld)
where
import State
import World
import Graphics.Gloss
import Geometry.Intersection
import Geometry.Segment
import qualified Array		as A
import Array			(Array)
import Data.Maybe


drawState :: State -> Picture
drawState state
 	| ModeDisplayWorld 	<- stateModeDisplay state
 	= drawWorldWithViewPos (stateViewPos state) (stateWorld state)

	| ModeDisplayNormalised <- stateModeDisplay state
	= drawWorldWithViewPos (0, 0) 
	$ normaliseWorld (stateViewPos state)
	$ stateWorld state

	| otherwise
	= Blank
	

drawWorldWithViewPos :: Point -> World -> Picture
drawWorldWithViewPos (px, py) world
 = let	
	-- the world 
	picWorld	= Color white
			$ drawWorld world

	-- view position indicator
	picDude		= Color green
			$ Translate px py
			$ ThickCircle 2 4

	-- crossings
	ptCrossings
		= catMaybes
		$ [ intersectSegHorzLine p1 p2 py
				| (_, p1, p2) <- A.toList $ worldSegments world ]

	picCrossings	= Pictures
			$ [ Color red
				$ Translate x y
				$ ThickCircle 1 2 | (x, y)	<- ptCrossings]


   in	Pictures [picWorld, picDude, picCrossings]


drawWorld :: World -> Picture
drawWorld world
	= drawSegments
	$ worldSegments world


drawSegments :: Array Segment -> Picture
drawSegments segments
	= Pictures
	$ map drawSegment
	$ A.toList 
	$ segments


drawSegment :: Segment -> Picture
drawSegment (_, (x1, y1), (x2, y2))
	= Line [(f x1, f y1), (f x2, f y2)]
	where	f	= fromRational . toRational
