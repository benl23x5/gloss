
-- A nifty animated fractal of a tree, superimposed on a background 
--      of three red rectangles.
import Graphics.Gloss

main :: IO ()
main 
 =      animate (InWindow "Zen" (800, 600) (5, 5))
                (greyN 0.2)
                frame   


-- Produce one frame of the animation.
frame :: Float -> Picture
frame timeS
 = Pictures     
        -- the red rectangles
        [ Translate 0 150       backRec
        , Translate 0 0         backRec
        , Translate 0 (-150)    backRec

        -- the tree
        , Translate 0 (-150) $  treeFrac 7 timeS
        ]


-- One of the red backing rectangles, with a white outline.
backRec :: Picture
backRec 
 = Pictures
        [ Color red     (rectangleSolid 400 100)
        , Color white   (rectangleWire  400 100) ]


-- The color for the outline of the tree's branches.
treeOutline :: Color
treeOutline     = makeColor 0.3 0.3 1.0 1.0


-- The color for the shading of the tree's branches.
--      The Alpha here is set to 0.5 so the branches are partly transparent.
treeColor :: Color
treeColor       = makeColor 0.0 1.0 0.0 0.5


-- The tree fractal.
--  The position of the branches changes depending on the animation time
--  as well as the iteration number of the fractal.
treeFrac :: Int -> Float -> Picture
treeFrac 0 timeS = Blank
treeFrac n timeS
 = Pictures
        [ Color treeColor       $ rectangleUpperSolid 20 300
        , Color treeOutline     $ rectangleUpperWire  20 300
        , Translate 0 30
                $ Rotate  (200 * sin timeS / (fromIntegral n) )
                $ Scale   0.9 0.9 
                $ treeFrac (n-1) timeS

        , Translate 0 70
                $ Rotate  (-200 * sin timeS / (fromIntegral n))
                $ Scale   0.8 0.8 
                $ treeFrac (n-1) timeS
        ]
