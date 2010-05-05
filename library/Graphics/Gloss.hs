
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
--   Animations and simulations can be constructed similarly using the `animateInWindow` 
--   and `simulateInWindow` functions. 
--
--   If you want to manage your own key\/mouse events then use gameInWindow from the
--   Graphics.Gloss.Game module.
--
--   Gloss uses OpenGL under the hood, but you don't have to worry about any of that.
--
-- @
--   Release Notes:
--   Since 1.0.0.2:
--     Added game mode.
--     Added QuadTree and Extent structures.
--     Added simple ray casting.
-- @
--
module Graphics.Gloss 
	( module Graphics.Gloss.Picture
	, module Graphics.Gloss.Color
	, module Graphics.Gloss.ViewPort
	, displayInWindow 
	, animateInWindow
	, simulateInWindow)
where
import Graphics.Gloss.Picture
import Graphics.Gloss.Color
import Graphics.Gloss.ViewPort
import Graphics.Gloss.Internals.Interface.Display	(displayInWindow)
import Graphics.Gloss.Internals.Interface.Animate	(animateInWindow)
import Graphics.Gloss.Internals.Interface.Simulate	(simulateInWindow)
