
module Graphics.Gloss 
	( Picture (..)
	, displayInWindow 
	, animateInWindow
	, simulateInWindow
	, module Graphics.Gloss.Color)
where

import Graphics.Gloss.Picture
import Graphics.Gloss.Color
import Graphics.Gloss.Internals.Interface.Display	(displayInWindow)
import Graphics.Gloss.Internals.Interface.Animate	(animateInWindow)
import Graphics.Gloss.Internals.Interface.Simulate	(simulateInWindow)
