
import Graphics.Gloss

main =  displayInWindow
		"Tree Fractal"
		(700, 800) 
		(20,  20)
		(picture 4)


-- The picture is a tree fractal, graded from brown to green
picture :: Int -> Picture	
picture degree
	= Translate 0 (-300)
	$ tree degree brown


-- Basic shape, but coloured
stump :: Color -> Picture
stump color 
	= Color color
	$ Polygon [(30,0), (15,300), (-15,300), (-30,0)]


-- Tree Fractal function.  First argument is the fractal degree
tree :: Int -> Color -> Picture
tree 0 color = stump color
tree n color 
 = let	smallTree 
		= Scale 0.5 0.5 
		$ tree (n-1) (greener color)
   in	Pictures
		[ stump color
		, Translate 0 300 $ smallTree
		, Translate 0 240 $ Rotate 20	 smallTree
		, Translate 0 180 $ Rotate (-20) smallTree
		, Translate 0 120 $ Rotate 40 	 smallTree
		, Translate 0  60 $ Rotate (-40) smallTree ]
		

-- A starting colour for the stump
brown :: Color
brown = RGBA8 139 100 35  230


-- A dodgy function to change the colour by degrees
greener :: Color -> Color
greener (RGBA8 r g b a) = RGBA8 r (g+16) b a

