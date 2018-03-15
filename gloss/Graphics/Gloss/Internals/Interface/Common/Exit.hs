{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes    #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Callback for exiting the program.
module Graphics.Gloss.Internals.Interface.Common.Exit
        (callback_exit)
where
import Graphics.Gloss.Internals.Interface.Backend.Types

callback_exit :: a -> Callback
callback_exit stateRef
 =      KeyMouse (keyMouse_exit stateRef)

keyMouse_exit :: a -> KeyboardMouseCallback
keyMouse_exit
        _
        backend
        key keyState _
        _
        | key           == SpecialKey KeyEsc
        , keyState      == Down
        = exitBackend backend

        | otherwise
        = return ()
