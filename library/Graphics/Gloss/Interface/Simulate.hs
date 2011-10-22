
-- We export this stuff separately so we don't clutter up the 
-- API of the Graphics.Gloss module.

-- | Simulate mode is for producing an animation of some model who's picture
--   changes over finite time steps. The behavior of the model can also depent
--   on the current `ViewPort`.
module Graphics.Gloss.Interface.Simulate
 	( module Graphics.Gloss.Data.Picture
	, module Graphics.Gloss.Data.Color
	, ViewPort(..)
	, simulateInWindow)
where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.ViewPort
import Graphics.Gloss.Internals.Interface.Simulate