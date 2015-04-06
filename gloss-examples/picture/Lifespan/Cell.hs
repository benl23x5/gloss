module Cell where

import Graphics.Gloss

data Cell = Cell Point    -- centre
                 Float    -- radius
                 Int      -- remaining lifetime
                 deriving Show

-- Produce a new cell of a certain relative radius at a certain angle.
-- The factor argument is in the range [0..1] so spawned cells are
-- smaller than their parent.
-- The check whether it fits in the community is elsewhere.
offspring :: Cell -> Float -> Float -> Int -> Cell
offspring (Cell (x,y) r _) alpha factor lifespan =
    Cell (x + (childR+r) * cos alpha, y + (childR+r) * sin alpha)
         childR 
         lifespan
    where childR = factor * r

-- Do two cells overlap?         
-- Used to decide if newly spawned cells can join the community.
overlap :: Cell -> Cell -> Bool
overlap (Cell (x1,y1) r1 _) (Cell (x2,y2) r2 _) = centreDist < (r1 + r2) *0.999
    where centreDist = sqrt(xdiff*xdiff + ydiff*ydiff)
          xdiff = x1 - x2
          ydiff = y1 - y2

-- thickness of circle is determined by lifespan
render :: Cell -> Picture
render (Cell (x,y) r life) 
        = Color (makeColor 0.6 z 0.6 1.0)
        $ Translate x y
        $ ThickCircle (r - thickness / 2) thickness
        where   z               = fromIntegral life * 0.12
                thickness       = fromIntegral life
