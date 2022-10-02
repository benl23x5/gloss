module Cell where

import Graphics.Gloss

data Cell = Cell
  { centre  :: Point
  , radius  :: Float
  , lifetime :: Int
  } deriving Show

{- | Produce a new cell of a certain relative radius at a certain angle.
The factor argument is in the range [0..1] so spawned cells are
smaller than their parent.
The check whether it fits in the community is elsewhere.
-}
offspring :: Cell -> Float -> Float -> Int -> Cell
offspring Cell{centre=(x, y), radius=r} angle factor lifespan =
    Cell
      (x + dist * cos angle, y + dist * sin angle)
      childRadius
      lifespan
    where
        childRadius = factor * r
        dist= r + childRadius


distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  sqrt((x1-x2)**2 + (y1 - y2)**2)

{- | Do two cells overlap?
Used to decide if newly spawned cells can join the community.
-}
overlap :: Cell -> Cell -> Bool
overlap
  Cell{centre=c1, radius=r1}
  Cell{centre=c2, radius=r2} =
    distance c1 c2 < (r1 + r2) * 0.999


alive :: Cell -> Bool
alive Cell{lifetime=l} = l >= 0

age :: Cell -> Cell
age c@Cell{lifetime=l} = c{lifetime=l-1}

-- | thickness of circle is determined by lifespan
render :: Cell -> Picture
render (Cell (x,y) r life) =
  Color (makeColor 0.6 z 0.6 1.0)
  . Translate x y
  . ThickCircle (r - thickness / 2)
  $ thickness
    where
      z = fromIntegral life * 0.12
      thickness = fromIntegral life
