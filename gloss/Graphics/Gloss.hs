
-- | Gloss hides the pain of drawing simple vector graphics behind a nice data type and
--      a few display functions.
--
--   Getting something on the screen is as easy as:
--
--  @
--  import Graphics.Gloss
--  main = `display` (InWindow \"Nice Window\" (200, 200) (10, 10)) `white` (`Circle` 80)
--  @
--
--   Once the window is open you can use the following:
--
-- @
-- * Quit
--   - esc-key
--
-- * Move Viewport
--   - arrow keys
--   - left-click drag
--
-- * Zoom Viewport
--   - page up/down-keys
--   - control-left-click drag
--   - right-click drag
--   - mouse wheel
--
-- * Rotate Viewport
--   - home/end-keys
--   - alt-left-click drag
--
-- * Reset Viewport
--   'r'-key
-- @
--
--
--   Animations can be constructed similarly using the `animate`.
--
--   If you want to run a simulation based around finite time steps then try
--   `simulate`.
--
--   If you want to manage your own key\/mouse events then use `play`.
--
--   Gloss uses OpenGL under the hood, but you don't have to worry about any of that.
--
--   Gloss programs should be compiled with @-threaded@, otherwise the GHC runtime
--   will limit the frame-rate to around 20Hz.
--
--   To build gloss using the GLFW window manager instead of GLUT use
--        @cabal install gloss --flags=\"GLFW -GLUT\"@
--
-- @
-- Release Notes:
--
--  For 1.11.1:
--   Thanks to Lars Wyssard
--   * Use default display resolution in full-screen mode.
--
--  For 1.10.1:
--   * Gloss no longer consumes CPU time when displaying static pictures.
--   * Added displayIO wrapper for mostly static pictures, eg when
--     plotting graphs generated from infrequently updated files.
--   * Allow viewport to be scaled with control-left-click drag.
--   * Rotation of viewport changed to alt-left-click drag.
--   * Preserve current colour when rendering bitmpaps.
--   * Changed to proper sum-of-squares colour mixing, rather than naive
--     addition of components which was causing mixed colours to be too dark.
--  Thanks to Thomas DuBuisson
--   * Allow bitmaps to be specified in RGBA byte order as well as ABGR.
--  Thanks to Gabriel Gonzalez
--   * Package definitions for building with Stack.
-- @
--
-- For more information, check out <http://gloss.ouroborus.net>.
--
module Graphics.Gloss
        ( module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , module Graphics.Gloss.Data.Bitmap
        , Display(..)
        , display
        , animate
        , simulate
        , play)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Interface.Pure.Animate
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Game

