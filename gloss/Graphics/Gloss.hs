
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
--   `simulateInWindow`.
--
--   If you want to manage your own key\/mouse events then use `gameInWindow`.
--
--   Gloss uses OpenGL under the hood, but you don't have to worry about any of that.
--
-- @Release Notes:
--
-- For 1.5.0:
--   * O(1) Conversion of ForeignPtrs to bitmaps.
--   * An extra flag on the Bitmap constructor allows bitmaps to be cached
--     in texture memory between frames.
--
-- For 1.4.0:
--   Thanks to Christiaan Baaij: 
--   * Refactoring of Gloss internals to support multiple window manager backends.
--   * Support for using GLFW as the window library instead of GLUT.
--     GLUT is still the default, but to use GLFW install gloss with:
--        cabal install gloss --flags=\"GLFW -GLUT\"
--    
-- For 1.3.0:
--   * Various wibbles for GHC 7.1   
--   Thanks to Ben Lambert-Smith:
--   * Support for displaying bitmap files.
--
-- For 1.2.0:
--   * Cleaned up module hierarchy.
--   * Added line-line intersection utils.
--   * Enabled -Wall and fixed all warnings.
--   * Various wibbles for GHC 7.0
--   Thanks to Thomas DuBuisson:
--   * Fixed a bug causing motion events to give the wrong position.
--   Thanks to Stephan Maka:
--   * Fixed a space leak in simulate mode when the window was hidden.
--
-- For 1.1.0:
--   * Added game mode.
--   * Added QuadTree and Extent structures.
--   * Added simple ray casting.
-- @
--
module Graphics.Gloss 
	( module Graphics.Gloss.Data.Picture
	, module Graphics.Gloss.Data.Color
	, displayInWindow
	, animateInWindow
        , simulateInWindow
	, gameInWindow)
where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Display
import Graphics.Gloss.Internals.Interface.Animate
import Graphics.Gloss.Internals.Interface.Simulate
import Graphics.Gloss.Internals.Interface.Game