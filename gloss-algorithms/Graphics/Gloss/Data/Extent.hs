{-# LANGUAGE PatternGuards #-}

-- | Represents an integral rectangular area of the 2D plane.
--   Using `Int`s (instead of `Float`s) for the bounds means we can safely
--   compare extents for equality.
module Graphics.Gloss.Data.Extent
        ( Extent
        , Coord
        , makeExtent
        , takeExtent
        , squareExtent
        , sizeOfExtent
        , isUnitExtent
        , coordInExtent
        , pointInExtent
        , centerCoordOfExtent
        , cutQuadOfExtent
        , quadOfCoord
        , pathToCoord
        , intersectSegExtent
        , touchesSegExtent)
where
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Quad
import Graphics.Gloss.Geometry.Line
import Data.Maybe


-- | A rectangular area of the 2D plane.
--   We keep the type abstract to ensure that invalid extents cannot be
--   constructed.
data Extent
        = Extent Int Int Int Int
        deriving (Eq, Show)


-- | An integral coordinate.
type Coord
        = (Int, Int)


-- | Construct an extent.
--      The north value must be > south, and east > west, else `error`.
makeExtent
        :: Int  -- ^ y max (north)
        -> Int  -- ^ y min (south)
        -> Int  -- ^ x max (east)
        -> Int  -- ^ x min (west)
        -> Extent

makeExtent n s e w
        | n >= s, e >= w
        = Extent n s e w

        | otherwise
        = error "Graphics.Gloss.Geometry.Extent.makeExtent: invalid extent"


-- | Take the NSEW components of an extent.
takeExtent :: Extent -> (Int, Int, Int, Int)
takeExtent (Extent n s e w)
        = (n, s, e, w)


-- | A square extent of a given size.
squareExtent :: Int -> Extent
squareExtent i
        = Extent i 0 i 0


-- | Get the width and height of an extent.
sizeOfExtent :: Extent -> (Int, Int)
sizeOfExtent (Extent n s e w)
        = (e - w, n - s)


-- | Check if an extent is a square with a width and height of 1.
isUnitExtent :: Extent -> Bool
isUnitExtent extent
        = sizeOfExtent extent == (1, 1)


-- | Check whether a coordinate lies inside an extent.
coordInExtent :: Extent -> Coord -> Bool
coordInExtent (Extent n s e w) (x, y)
        =  x >= w && x < e
        && y >= s && y < n


-- | Check whether a point lies inside an extent.
pointInExtent :: Extent -> Point -> Bool
pointInExtent (Extent n s e w) (x, y)
 = let  n'      = fromIntegral n
        s'      = fromIntegral s
        e'      = fromIntegral e
        w'      = fromIntegral w

   in   x >= w' && x <= e'
     && y >= s' && y <= n'


-- | Get the coordinate that lies at the center of an extent.
centerCoordOfExtent :: Extent -> (Int, Int)
centerCoordOfExtent (Extent n s e w)
 =      ( w + (e - w) `div` 2
        , s + (n - s) `div` 2)


-- | Cut one quadrant out of an extent.
cutQuadOfExtent :: Quad -> Extent -> Extent
cutQuadOfExtent quad (Extent n s e w)
 = let  hheight = (n - s) `div` 2
        hwidth  = (e - w) `div` 2
   in   case quad of
                NW -> Extent n (s + hheight)  (e - hwidth) w
                NE -> Extent n (s + hheight)  e (w + hwidth)
                SW -> Extent (n - hheight) s  (e - hwidth) w
                SE -> Extent (n - hheight) s  e (w + hwidth)


-- | Get the quadrant that this coordinate lies in, if any.
quadOfCoord :: Extent -> Coord -> Maybe Quad
quadOfCoord extent coord
        = listToMaybe
        $ filter (\q -> coordInExtent (cutQuadOfExtent q extent) coord)
        $ allQuads


-- | Constuct a path to a particular coordinate in an extent.
pathToCoord :: Extent -> Coord -> Maybe [Quad]
pathToCoord extent coord
        | isUnitExtent extent
        = Just []

        | otherwise
        = do    quad    <- quadOfCoord extent coord
                rest    <- pathToCoord (cutQuadOfExtent quad extent) coord
                return  $ quad : rest


-- | If a line segment (P1-P2) intersects the outer edge of an extent then
--   return the intersection point, that is closest to P1, if any.
--   If P1 is inside the extent then `Nothing`.
--
--   @
--                   P2
--                  /
--            ----/-
--            | /  |
--            +    |
--           /------
--         /
--        P1
--   @
--
intersectSegExtent :: Point -> Point -> Extent -> Maybe Point
intersectSegExtent p1@(x1, y1) p2 (Extent n' s' e' w')
        -- starts below extent
        | y1 < s
        , Just pos      <- intersectSegHorzSeg p1 p2 s w e
        = Just pos

        -- starts above extent
        | y1 > n
        , Just pos      <- intersectSegHorzSeg p1 p2 n w e
        = Just pos

        -- starts left of extent
        | x1 < w
        , Just pos      <- intersectSegVertSeg p1 p2 w s n
        = Just pos

        -- starts right of extent
        | x1 > e
        , Just pos      <- intersectSegVertSeg p1 p2 e s n
        = Just pos

        -- must be starting inside extent.
        | otherwise
        = Nothing

        where   n       = fromIntegral n'
                s       = fromIntegral s'
                e       = fromIntegral e'
                w       = fromIntegral w'


-- | Check whether a line segment's endpoints are inside an extent, or if it
--   intersects with the boundary.
touchesSegExtent :: Point -> Point -> Extent -> Bool
touchesSegExtent p1 p2 extent
        =   pointInExtent extent p1
         || pointInExtent extent p2
         || isJust (intersectSegExtent p1 p2 extent)

