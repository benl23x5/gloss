{-# LANGUAGE PatternGuards #-}

-- | Geometric functions concerning lines.
module Graphics.Gloss.Geometry.Line
	( closestPointOnLine
	, closestPointOnLineParam 
	, intersectSegHorzSeg
	, intersectSegVertSeg)
where
import Graphics.Gloss.Picture	(Point)
import Graphics.Gloss.Geometry.Vector

-- | Given an infinite line which intersects `P1` and `P1`,
--	return the point on that line that is closest to `P3`
closestPointOnLine
	:: Point 	-- ^ `P1`
	-> Point	-- ^ `P2`
	-> Point	-- ^ `P3`
	-> Point	-- ^ the point on the line P1-P2 that is closest to `P3`

{-# INLINE closestPointOnLine #-}

closestPointOnLine p1 p2 p3
 	= p1 + (u `mulSV` (p2 - p1))
	where	u	= closestPointOnLineParam p1 p2 p3


-- | Given an infinite line which intersects P1 and P2,
--	let P4 be the point on the line that is closest to P3.
--
--	Return an indication of where on the line P4 is relative to P1 and P2.
--
-- @
--      if P4 == P1 then 0
--      if P4 == P2 then 1
--      if P4 is halfway between P1 and P2 then 0.5
-- @
--
-- @
--        |
--       P1
--        | 
--     P4 +---- P3      
--        |
--       P2
--        |
-- @
--
{-# INLINE closestPointOnLineParam #-}
closestPointOnLineParam
	:: Point 	-- ^ `P1`
	-> Point 	-- ^ `P2`
	-> Point 	-- ^ `P3`
	-> Float

closestPointOnLineParam p1 p2 p3
 	= (p3 - p1) `dotV` (p2 - p1) 
 	/ (p2 - p1) `dotV` (p2 - p1)


-- | Check if line segment (P1-P2) clears a box (P3-P4) by being well outside it.
segClearsBox 
	:: Point	-- ^ P1 First point of segment. 
	-> Point 	-- ^ P2 Second point of segment.
	-> Point 	-- ^ P3 Lower left point of box.
	-> Point	-- ^ P4 Upper right point of box.
	-> Bool

segClearsBox (x1, y1) (x2, y2) (xa, ya) (xb, yb)
	| x1 < xa, x2 < xa	= True
	| x1 > xb, x2 > xb	= True
	| y1 < ya, y2 < ya	= True
	| y1 > yb, y2 > yb	= True
	| otherwise		= False


-- | Check if an arbitrary segment intersects a horizontal segment.
--
-- @
--                 P2
--                /
--   (xa, y3) ---+---- (xb, y3)
--              /
--             /
--           P1
-- @ 

intersectSegHorzSeg
	:: Point 	-- ^ P1 First point of segment.
	-> Point 	-- ^ P2 Second point of segment.
	-> Float 	-- ^ (y3) y value of horizontal segment.
	-> Float        -- ^ (xa) Leftmost x value of horizontal segment.
	-> Float 	-- ^ (xb) Rightmost x value of horizontal segment.
	-> Maybe Point	-- ^ (x3, y3) Intersection point, if any.
	
intersectSegHorzSeg p1@(x1, y1) p2@(x2, y2) y0 xa xb
	| segClearsBox p1 p2 (xa, y0) (xb, y0)
	= Nothing

	| x0 < xa	= Nothing
	| x0 > xb	= Nothing
	| otherwise	= Just (x0, y0)
		
	where x0 | (y2 - y1) == 0 	= x1
		 | otherwise		= (y0 - y1) * (x2 - x1) / (y2 - y1) + x1


-- | Check if an arbitrary segment intersects a vertical segment.
--
-- @
--         (x3, yb)
--               |   P1
--               | /
--               +
--             / |
--          P2   |
--             (x3, ya)
-- @ 

intersectSegVertSeg
	:: Point	-- ^ P1 First point of segment.
	-> Point 	-- ^ P2 Second point of segment.
	-> Float 	-- ^ (x3) x value of vertical segment
	-> Float	-- ^ (ya) Lowest y value of vertical segment.
	-> Float	-- ^ (yb) Highest y value of vertical segment.
	-> Maybe Point	-- ^ (x3, y3) Intersection point, if any.

intersectSegVertSeg p1@(x1, y1) p2@(x2, y2) x0 ya yb
	| segClearsBox p1 p2 (x0, ya) (x0, yb)
	= Nothing
	
	| y0 < ya	= Nothing
	| y0 > yb	= Nothing
	| otherwise	= Just (x0, y0)
	
	where y0 | (x2 - x1) == 0	= y1
		 | otherwise		= (x0 - x1) * (y2 - y1) / (x2 - x1) + y1
