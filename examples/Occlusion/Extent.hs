
module Extent
	( Extent (..)
	, Coord
	, squareExtent
	, cutQuadExtent
	, isInExtent 
	, sizeOfExtent
	, insertNodeByCoord
	, flattenQuadTree)
where
import QuadTree
import Data.Maybe

data Extent
	= Extent
	{ extentNorth	:: Int
	, extentSouth	:: Int
	, extentEast	:: Int
	, extentWest	:: Int }
	deriving (Eq, Show)

type Coord
	= (Int, Int)


-- | Make a square extent of a given size.
squareExtent :: Int -> Extent
squareExtent i
	= Extent i 0 i 0


-- | Get the width and height of an extent.
sizeOfExtent :: Extent -> (Int, Int)
sizeOfExtent (Extent n s e w)
	= (e - w, n - s)


-- | Get the position of the center of an extent.
centerCoordOfExtent :: Extent -> (Int, Int)
centerCoordOfExtent (Extent n s e w)
 = 	( w + (e - w) `div` 2
	, s + (n - s) `div` 2)
	
	
-- | Check if an extent is a square with a width and height of 1
isUnitExtent :: Extent -> Bool
isUnitExtent extent
	= sizeOfExtent extent == (1, 1)

	
-- | Cut one quadrant out of an extent.
cutQuadExtent :: Quad -> Extent -> Extent
cutQuadExtent quad (Extent n s e w)  
 = let	hheight	= (n - s) `div` 2
	hwidth	= (e - w) `div` 2
   in	case quad of
	  	NW -> Extent n (s + hheight)  (e - hwidth) w
		NE -> Extent n (s + hheight)  e (w + hwidth)
		SW -> Extent (n - hheight) s  (e - hwidth) w
		SE -> Extent (n - hheight) s  e (w + hwidth)
	
	
-- | Check if a position lies inside an extent.
isInExtent :: Extent -> Coord -> Bool
isInExtent (Extent n s e w) (x, y)
	=  x >= w && x < e
	&& y >= s && y < n


-- | Get the quadrant that this position lies in, if any.
quadOfCoord :: Extent -> Coord -> Maybe Quad
quadOfCoord extent coord
 	= listToMaybe 
	$ filter (\q -> isInExtent (cutQuadExtent q extent) coord)
	$ allQuads

	
-- | Get the path to a position in an extent.
pathOfCoord :: Extent -> Coord -> Maybe [Quad]
pathOfCoord extent coord
	| isUnitExtent extent	
	= Just []
	
	| otherwise
	= do	quad	<- quadOfCoord extent coord
		rest	<- pathOfCoord (cutQuadExtent quad extent) coord
		return	$ quad : rest
		

-- | Insert a node into the tree based on a position.
insertNodeByCoord :: Extent -> Coord -> a -> QuadTree a -> Maybe (QuadTree a)
insertNodeByCoord extent coord x tree
 = do	path	<- pathOfCoord extent coord
	return	$  insertNodeByPath path x tree
	

-- | Get a list of positions and elements from a QuadTree
flattenQuadTree :: Extent -> QuadTree a -> [(Coord, a)]
flattenQuadTree extentInit treeInit
 = flatten' extentInit treeInit (centerCoordOfExtent extentInit)
 where	flatten' extent tree coord
 	 = case tree of
		TNil	-> []
		TLeaf x	-> [(coord, x)]
		TNode{}	-> concat $ map (flattenQuad extent tree coord) allQuads
			
	flattenQuad extent tree coord quad
 	 = let	extent'		= cutQuadExtent quad extent
		Just tree'	= takeQuad quad tree
		coord'		= centerCoordOfExtent extent'
   	   in	flatten' extent' tree' coord'



