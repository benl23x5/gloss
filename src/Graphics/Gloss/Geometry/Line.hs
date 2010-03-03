
module Graphics.Gloss.Geometry.Line
	( closestPointOnLine
	, closestPointOnLine_param )
where
import Graphics.Gloss.Picture	(Point)
import Graphics.Gloss.Geometry.Vector

-- | Given an infinite line which intersects p1 and p2
--	return the point on that line which is closest to p3
{-# INLINE closestPointOnLine #-}
closestPointOnLine
	:: Point -> Point	-- p1 p2	(the line)
	-> Point		-- p3		(the point)
	-> Point		-- the point on the line which is closest to p3

closestPointOnLine p1 p2 p3
 	= p1 + (u `mulSV` (p2 - p1))
	where	u	= closestPointOnLine_param p1 p2 p3


-- | Given an infinite line which intersects p1 and p2
--	pC is the point on the line which is closest to p3
--
--	u is the parameter indicating where on the line pC is relative to p1 and p2.
--	
--	ie 	if pC == p1	then u = 0
--		if pC == p2	then u = 1
--		if pC is halfway between p1 and p2 then u = 0.5
--
{-# INLINE closestPointOnLine_param #-}
closestPointOnLine_param :: Point -> Point -> Point -> Float
closestPointOnLine_param p1 p2 p3
 	= (p3 - p1) `dot` (p2 - p1) 
 	/ (p2 - p1) `dot` (p2 - p1)
