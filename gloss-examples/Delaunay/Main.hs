
import QuickHull
import QuickSort
import TestData
import Graphics.Gloss
import Graphics.Gloss.Shapes
import qualified Data.Vector.Unboxed		as V
import Data.Function

type Vec	= V.Vector

main	= displayInWindow
		"Delaunay"
		(1000, 1000)
		(1200,  10)
		white
		picture
		
picture	
 = let	-- points	= genPointsDisc    1000 (0, 0) 400
	points	= genPointsUniform 1000 (-400) 400
	hull	= quickHull points

	psSorted	= quickSortBy (compare `on` fst) points
	ixMedian	= V.length psSorted `div` 2
	pMedian		= psSorted V.! ixMedian
	
   in	Pictures
		[ pictureOfPoints points
		, Color blue $ pictureOfPath        hull
		, Color red  $ pictureOfPointCircle pMedian]


-- | Display a vector of points as a picture.
--   Each point is displayed as a square.
pictureOfPoints :: Vec (Double, Double) -> Picture
pictureOfPoints vec
	= Pictures
 		$ map pictureOfPoint
		$ V.toList vec
	
	
-- | Display a single point as a picture.
pictureOfPoint :: (Double, Double) -> Picture
pictureOfPoint (x, y)
	= Translate (doubleToFloat x) (doubleToFloat y) point
	where	point		= rectangleSolid 3 3

	
-- | Display a single point with a circle around it
pictureOfPointCircle :: (Double, Double) -> Picture
pictureOfPointCircle (x, y)
	= Translate (doubleToFloat x) (doubleToFloat y) point
	where	point	= Pictures
				[ rectangleSolid 3 3
				, Circle 10 ]

	
-- | Display a vector of points as a path.
pictureOfPath :: Vec (Double, Double) -> Picture
pictureOfPath vec
 = lineLoop
	$ map (\(x, y) -> (doubleToFloat x, doubleToFloat y))
	$ V.toList vec
 
	
doubleToFloat	= fromRational . toRational