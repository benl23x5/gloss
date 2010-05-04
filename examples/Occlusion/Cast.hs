{-# LANGUAGE PatternGuards #-}

module Cast
	( Ray
	, cast
	, rayClearsBox
	, rayTouchesExtent
	, rayIntersectsHorzSeg
	, rayIntersectsVertSeg)
where
import Extent
import QuadTree
import Data.Maybe


type Ray	= (Pos, Pos)


cast :: Ray -> Extent -> QuadTree a -> [(Pos, Extent, a)]
cast ray extent tree
 = case tree of
	TNil	-> []
	TLeaf a
	 -> case rayIntersectsExtent ray extent of
		Just pos	-> [(pos, extent, a)]
		Nothing		-> []
					
	TNode nw ne sw se
	 | rayTouchesExtent ray extent 
	 -> concat
	 	[ cast ray (cutQuadExtent NW extent) nw
		, cast ray (cutQuadExtent NE extent) ne
		, cast ray (cutQuadExtent SW extent) sw
		, cast ray (cutQuadExtent SE extent) se ]
	
	_ -> []
		
	
rayIntersectsExtent :: Ray -> Extent -> Maybe Pos
rayIntersectsExtent ray@((x0, y0), _) extent@(Extent n' s' e' w')
	= listToMaybe 
	$ catMaybes
		[ if y0 < s then rayIntersectsHorzSeg ray s w e else Nothing
     	 	, if y0 > n then rayIntersectsHorzSeg ray n w e else Nothing
     	 	, if x0 < w then rayIntersectsVertSeg ray w s n else Nothing
		, if x0 > e then rayIntersectsVertSeg ray e s n else Nothing ]

	where	n	= fromIntegral n'
		s	= fromIntegral s'
		e	= fromIntegral e'
		w	= fromIntegral w'

		
	
rayTouchesExtent :: Ray -> Extent -> Bool
rayTouchesExtent ray@(p1, p2) extent@(Extent n' s' e' w')
	=   posInExtent extent p1
       	 || posInExtent extent p2
	 || isJust (rayIntersectsExtent ray extent)
	

-- | Check if a ray clears a box by being well outside it.
rayClearsBox :: Ray -> Float -> Float -> Float -> Float -> Bool
rayClearsBox ((x1, y1), (x2, y2)) xa ya xb yb
	| x1 < xa, x2 < xa	= True
	| x1 > xb, x2 > xb	= True
	| y1 < ya, y2 < ya	= True
	| y1 > yb, y2 > yb	= True
	| otherwise		= False


-- | Check if a ray intersects a horizontal line segment.
rayIntersectsHorzSeg 
	:: Ray 
	-> Float 	-- ^ y val of segment
	-> Float        -- ^ leftmost  x val of segment
	-> Float 	-- ^ rightmost x val of segment
	-> Maybe Pos	-- the intersection point, if any.
	
rayIntersectsHorzSeg ray@((x1, y1), (x2, y2)) y0 xa xb
	| rayClearsBox ray xa y0 xb y0
	= Nothing

	| x0 < xa	= Nothing
	| x0 > xb	= Nothing
	| otherwise	= Just (x0, y0)
		
	where x0 | (y2 - y1) == 0 	= x1
		 | otherwise		= (y0 - y1) * (x2 - x1) / (y2 - y1) + x1


-- | Check if a ray intesects a vertical line segment.
rayIntersectsVertSeg 
	:: Ray 
	-> Float 	-- ^ x val of segment
	-> Float	-- ^ downmost y val of segment
	-> Float	-- ^ upmost   y val of segment
	-> Maybe Pos	-- the intersection point, if any.

rayIntersectsVertSeg ray@((x1, y1), (x2, y2)) x0 ya yb
	| rayClearsBox ray x0 ya x0 yb
	= Nothing
	
	| y0 < ya	= Nothing
	| y0 > yb	= Nothing
	| otherwise	= Just (x0, y0)
	
	where y0 | (x2 - x1) == 0	= y1
		 | otherwise		= (x0 - x1) * (y2 - y1) / (x2 - x1) + y1
