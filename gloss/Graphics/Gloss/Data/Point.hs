{-# OPTIONS -fno-warn-missing-methods -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Gloss.Data.Point
	( Point
	, pointInBox)
where

-- | A point on the x-y plane.
--   Points can also be treated as `Vector`s, so "Graphics.Gloss.Data.Vector"
--   may also be useful.
type Point	= (Float, Float)			


-- | Pretend a point is a number.
--	Vectors aren't real numbes according to Haskell, because they don't
--	support the multiply and divide field operators. We can pretend they
--	are though, and use the (+) and (-) operators as component-wise
--	addition and subtraction.
--
instance Num Point where
	(+) (x1, y1) (x2, y2)	= (x1 + x2, y1 + y2)
 	(-) (x1, y1) (x2, y2)	= (x1 - x2, y1 - y2)
        (*) (x1, y1) (x2, y2)   = (x1 * x2, y1 * y2)
        signum (x, y)           = (signum x, signum y)
        abs    (x, y)           = (abs x, abs y)
	negate (x, y)		= (negate x, negate y)	
        fromInteger x           = (fromInteger x, fromInteger x)


-- | Test whether a point lies within a rectangular box that is oriented
--   on the x-y plane. The points P1-P2 are opposing points of the box,
--   but need not be in a particular order.
--
-- @
--    P2 +-------+
--       |       |
--       | + P0  |
--       |       |
--       +-------+ P1
-- @
--
pointInBox 
	:: Point 
	-> Point 
	-> Point -> Bool
	
pointInBox (x0, y0) (x1, y1) (x2, y2)
 	=  x0 >= min x1 x2
 	&& x0 <= max x1 x2
	&& y0 >= min y1 y2
	&& y0 <= max y1 y2
