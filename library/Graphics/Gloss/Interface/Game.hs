
-- We export this stuff separately so we don't clutter up the 
-- API of the Graphics.Gloss module.

-- | This game mode lets you manage your own input. Pressing ESC will still abort the program,
--   but you don't get automatic pan and zoom controls like with `displayInWindow`.
module Graphics.Gloss.Interface.Game
 	( module Graphics.Gloss.Data.Picture
	, module Graphics.Gloss.Data.Color
	, gameInWindow
	, Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Game
import Graphics.Gloss.Internals.Interface.Backend
