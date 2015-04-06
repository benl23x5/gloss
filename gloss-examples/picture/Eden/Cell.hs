module Cell where

import Graphics.Gloss

data Cell 
        = Cell  Point    -- centre
                Float    -- radius
                Int     
                deriving Show

-- Produce a new cell of a certain relative radius at a certain angle.
--      The factor argument is in the range [0..1] so spawned cells are
--      smaller than their parent.
-- The check whether it fits in the community is elsewhere.
offspring :: Cell -> Float -> Float -> Cell
offspring (Cell (x,y) r gen) alpha factor 
        = Cell  (x + (childR+r) * cos alpha, y + (childR+r) * sin alpha) 
                childR 
                (gen + 1)

        where childR = factor * r

-- Do two cells overlap?         
-- Used to decide if newly spawned cells can join the community.
overlap :: Cell -> Cell -> Bool
overlap (Cell (x1,y1) r1 _) (Cell (x2,y2) r2 _) 
        = centreDist < (r1 + r2) * 0.999
        where   centreDist = sqrt(xdiff*xdiff + ydiff*ydiff)
                xdiff = x1 - x2
                ydiff = y1 - y2

render :: Cell -> Picture
render (Cell (x,y) r gen) 
 = let  z       = fromIntegral gen * 0.1
        color   = makeColor 0.0 z 0.5 1.0
   in   Color color
                $ Translate x y
                $ Circle r
