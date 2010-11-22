
-- | Gloss hides the pain of drawing simple vector graphics behind a nice data type and
--	a few display functions. 
--
--   Getting something on the screen is as easy as:
--
--  @
--    import Graphics.Gloss
--    main = `displayInWindow` \"My Window\" (200, 200) (10, 10) `white` (`Circle` 80)
--  @
--
--   Once the window is open you can use the following:
--
-- 	* Quit - esc-key.
--
--	* Move Viewport - left-click drag, arrow keys.
--
--	* Rotate Viewport - right-click drag, control-left-click drag, or home\/end-keys.
--
--	* Zoom Viewport - mouse wheel, or page up\/down-keys.
--
--   Animations can be constructed similarly using the `animateInWindow`.
--
--   If you want to run a simulation based around finite time steps then try
--   `simulateInWindow` from "Graphics.Gloss.Interface.Simulate".
--
--   If you want to manage your own key\/mouse events then use `gameInWindow` from
--   "Graphics.Gloss.Interface.Game".
--
--   Gloss uses OpenGL under the hood, but you don't have to worry about any of that.
--
-- @
--   Release Notes:
--   For 1.1.0.0:
--     Added game mode.
--     Added QuadTree and Extent structures.
--     Added simple ray casting.
-- @
--
module Graphics.Gloss 
	( module Graphics.Gloss.Data.Picture
	, module Graphics.Gloss.Data.Color
	, displayInWindow 
	, animateInWindow)
where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Display	(displayInWindow)
import Graphics.Gloss.Internals.Interface.Animate	(animateInWindow)
