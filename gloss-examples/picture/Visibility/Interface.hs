{-# LANGUAGE PatternGuards #-}
module Interface
        ( handleInput
        , stepState)
where
import State
import qualified Graphics.Gloss.Interface.Pure.Game     as G

-- Input ------------------------------------------------------------------------------------------
-- | Handle an input event.
handleInput :: G.Event -> State -> State

handleInput (G.EventKey key keyState _ (x, y)) state
        -- move the view position.
        | G.MouseButton G.LeftButton    <- key
        , G.Down                        <- keyState
        = state { stateModeInterface    = ModeInterfaceMove 
                , stateViewPos
                        = ( fromRational $ toRational x
                          , fromRational $ toRational y) }

        -- set the target position.
        | G.MouseButton G.RightButton   <- key
        , G.Down                        <- keyState
        = state { stateTargetPos
                        = Just ( fromRational $ toRational x
                               , fromRational $ toRational y) }

        | G.MouseButton G.LeftButton    <- key
        , G.Up                          <- keyState
        = state { stateModeInterface    = ModeInterfaceIdle }

handleInput (G.EventMotion (x, y)) state
        | stateModeInterface state == ModeInterfaceMove
        = state { stateViewPos
                        = ( fromRational $ toRational x
                          , fromRational $ toRational y) }

-- t : Turn target indicator off.
handleInput (G.EventKey key keyState _ _) state
        | G.Char 't'                    <- key
        , G.Down                        <- keyState
        = state { stateTargetPos        = Nothing }

-- w : Display the whole world.
handleInput (G.EventKey key keyState _ _) state
        | G.Char 'w'                    <- key
        , G.Down                        <- keyState
        = state { stateModeDisplay      = ModeDisplayWorld }

-- n : Display the normalised world.
handleInput (G.EventKey key keyState _ _) state
        | G.Char 'n'                    <- key
        , G.Down                        <- keyState
        = state { stateModeDisplay      = ModeDisplayNormalised }

-- a : Toggle approximate visibility
handleInput (G.EventKey key keyState _ _) state
        | G.Char 'a'                    <- key
        , G.Down                        <- keyState
        = state { stateModeOverlay
                        = case stateModeOverlay state of
                                ModeOverlayVisApprox    -> ModeOverlayNone
                                _                       -> ModeOverlayVisApprox }       

handleInput _ state
        = state

-- Step -------------------------------------------------------------------------------------------
-- | Advance the state one iteration
stepState :: Float -> State -> State
stepState _ state = state
