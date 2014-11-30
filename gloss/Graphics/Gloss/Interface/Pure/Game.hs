{-# LANGUAGE ExplicitForAll #-}

-- We export this stuff separately so we don't clutter up the 
-- API of the Graphics.Gloss module.

-- | This game mode lets you manage your own input. Pressing ESC will still abort the program,
--   but you don't get automatic pan and zoom controls like with `displayInWindow`.
module Graphics.Gloss.Interface.Pure.Game
 	( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
	, module Graphics.Gloss.Data.Color
	, play
	, Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Internals.Interface.Game
import Graphics.Gloss.Internals.Interface.Backend


-- | Play a game in a window. Like `simulate`, but you manage your own input events.
play    :: Display              -- ^ Display mode.
        -> Color                -- ^ Background color.
        -> Int                  -- ^ Number of simulation steps to take for each second of real time.
        -> world                -- ^ The initial world.
        -> (world -> Picture)   -- ^ A function to convert the world a picture.
        -> (Event -> world -> world)    
                -- ^ A function to handle input events.
        -> (Float -> world -> world)
                -- ^ A function to step the world one iteration.
                --   It is passed the period of time (in seconds) needing to be advanced.
        -> IO ()

play    display backColor simResolution
        worldStart worldToPicture worldHandleEvent worldAdvance

 = playWithBackendIO defaultBackendState 
        display backColor simResolution
        worldStart 
        (return . worldToPicture)
        (\event world -> return $ worldHandleEvent event world)
        (\time  world -> return $ worldAdvance     time  world)
        True
