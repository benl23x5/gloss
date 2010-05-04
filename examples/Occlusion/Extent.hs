
module Extent
	( Extent (..)
	, Pos
	, squareExtent
	, cutQuadExtent
	, isInExtent 
	, sizeOfExtent
	, insertNodeByPos
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

type Pos
	= (Int, Int)


-- | Make a square extent of a given size.
squareExtent :: Int -> Extent
squareExtent i
	= Extent i 0 i 0


-- | Get the width and height of an extent.
sizeOfExtent :: Extent -> (Int, Int)
sizeOfExtent (Extent n s e w)
	= (n - s, e - w)


-- | Get the position of the center of an extent.
centerPosOfExtent :: Extent -> (Int, Int)
centerPosOfExtent (Extent n s e w)
 = 	( s + (n - s) `div` 2
	, w + (e - w) `div` 2)
	
	
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
isInExtent :: Extent -> Pos -> Bool
isInExtent (Extent n s e w) (x, y)
	=  x >= w && x < e
	&& y >= s && y < n


-- | Get the quadrant that this position lies in, if any.
quadOfPos :: Extent -> Pos -> Maybe Quad
quadOfPos extent pos
 	= listToMaybe 
	$ filter (\q -> isInExtent (cutQuadExtent q extent) pos)
	$ allQuads

	
-- | Get the path to a position in an extent.
pathOfPos :: Extent -> Pos -> Maybe [Quad]
pathOfPos extent pos
	| isUnitExtent extent	
	= Just []
	
	| otherwise
	= do	quad	<- quadOfPos extent pos
		rest	<- pathOfPos (cutQuadExtent quad extent) pos
		return	$ quad : rest
		

-- | Insert a node into the tree based on a position.
insertNodeByPos	:: Extent -> Pos -> a -> QuadTree a -> Maybe (QuadTree a)
insertNodeByPos extent pos x tree
 = do	path	<- pathOfPos extent pos
	return	$  insertNodeByPath path x tree
	

-- | Get a list of positions and elements from a QuadTree
flattenQuadTree :: Extent -> QuadTree a -> [(Pos, a)]
flattenQuadTree extentInit treeInit
 = flatten' extentInit treeInit (centerPosOfExtent extentInit)
 where	flatten' extent tree pos
 	 = case tree of
		TNil	-> []
		TLeaf x	-> [(pos, x)]
		TNode{}	-> concat $ map (flattenQuad extent tree pos) allQuads
			
	flattenQuad extent tree pos quad
 	 = let	extent'		= cutQuadExtent quad extent
		Just tree'	= takeQuad quad tree
		pos'		= centerPosOfExtent extent'
   	   in	flatten' extent' tree' pos'






