
-- We export this stuff separately so we don't clutter up the 
-- API of the Graphics.Gloss module.

-- | This game mode lets you manage your own input. Pressing ESC will still abort the program,
--   but you don't get automatic pan and zoom controls like with `displayInWindow`.
module Graphics.Gloss.Game
 	( module Graphics.Gloss.Picture
	, module Graphics.Gloss.Color
	, module Graphics.Gloss.ViewPort
	, gameInWindow
	, Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Graphics.Gloss.Picture
import Graphics.Gloss.Color
import Graphics.Gloss.ViewPort
import Graphics.Gloss.Internals.Interface.Game		(gameInWindow, Event(..))
import Graphics.UI.GLUT.Callbacks.Window
