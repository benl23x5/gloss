
-- | Snowflake Fractal.
--      Based on ANUPlot code by Clem Baker-Finch.
--
import Graphics.Gloss

main = display (InWindow "Snowflake" (500, 500) (20,  20))
               black (picture 3)


-- Fix a starting edge length of 360
edge = 360 :: Float


-- Move the fractal into the center of the window and colour it nicely
picture :: Int -> Picture
picture degree 
        = Color aquamarine
        $ Translate (-edge/2) (-edge * sqrt 3/6)
        $ snowflake degree
        

-- The fractal function
side :: Int -> Picture
side 0 = Line [(0, 0), (edge, 0)]
side n 
 = let  newSide = Scale (1/3) (1/3) 
                $ side (n-1)
   in   Pictures
                [ newSide
                , Translate (edge/3) 0                    $ Rotate 60    newSide 
                , Translate (edge/2) (-(edge * sqrt 3)/6) $ Rotate (-60) newSide 
                , Translate (2 * edge/3) 0                $ newSide ]


-- Put 3 together to form the snowflake
snowflake :: Int -> Picture
snowflake n 
 = let  oneSide = side n
   in   Pictures
                [ oneSide 
                , Translate edge 0                      $ Rotate (-120) $ oneSide
                , Translate (edge/2) (edge * sqrt 3/2)  $ Rotate 120    $ oneSide]



