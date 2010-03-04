
-- | Geometric functions concerning lines.
module Graphics.Gloss.Geometry.Line
	( closestPointOnLine
	, closestPointOnLine_param )
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
	where	u	= closestPointOnLine_param p1 p2 p3


-- | Given an infinite line which intersects P1 and P2,
--	let P4 be the point on the line that is closest to P3.
--
--	Return an indication of where on the line P4 is relative to P1 and P2.
--
-- @	
--	if P4 == P1	  return 0
--	if P4 == P2	  return 1
--	if P4 is halfway between P1 and P2 then return 0.5
-- @
{-# INLINE closestPointOnLine_param #-}
closestPointOnLine_param 
	:: Point 	-- ^ `P1`
	-> Point 	-- ^ `P2`
	-> Point 	-- ^ `P3`
	-> Float

closestPointOnLine_param p1 p2 p3
 	= (p3 - p1) `dotV` (p2 - p1) 
 	/ (p2 - p1) `dotV` (p2 - p1)
