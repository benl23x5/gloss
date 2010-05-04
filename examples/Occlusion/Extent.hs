
module Extent
	( Extent (..)
	, cutQuadExtent
	, isInExtent )
where

data Extent
	= Extent
	{ extentNorth	:: Int
	, extentSouth	:: Int
	, extentEast	:: Int
	, extentWest	:: Int }
	deriving (Eq, Show)


-- | Cut one quadrant out of an extent.
cutQuadExtent :: Int -> Extent -> Extent
cutQuadExtent quad (Extent w e s n)  
 = let	hheight	= (n - 2) `div` 2
	hwidth	= (e - w) `div` 2
   in	case quad of
	 0 	-> Extent w (e - hwidth)  (s + hheight) n 
	 1	-> Extent (w + hwidth) e  (s + hheight) n 
	 2	-> Extent w (e - hwidth)  s (n - hheight)
	 3	-> Extent (w + hwidth) e  s (n - hheight)
	
	
-- | Check if a coordinate lies inside an extent.
isInExtent :: Extent -> (Int, Int) -> Bool
isInExtent (Extent w e s n) (x, y)
	=  x >= w && x < e
	&& y >= s && y < n

