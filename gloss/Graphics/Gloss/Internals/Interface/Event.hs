{-# LANGUAGE RankNTypes #-}
module Graphics.Gloss.Internals.Interface.Event
        ( Event (..)
        , keyMouseEvent
        , motionEvent )
where
import Data.IORef
import Graphics.Gloss.Internals.Interface.Backend

-- | Possible input events.
data Event
        = EventKey    Key KeyState Modifiers (Float, Float)
        | EventMotion (Float, Float)
        | EventResize (Int, Int)
        deriving (Eq, Show)

keyMouseEvent ::
        forall a . Backend a
        => IORef a
        -> Key
        -> KeyState
        -> Modifiers
        -> (Int, Int)
        -> IO Event
keyMouseEvent backendRef key keyState modifiers pos
        = EventKey key keyState modifiers <$> convertPoint backendRef pos

motionEvent ::
        forall a . Backend a
        => IORef a
        -> (Int, Int)
        -> IO Event
motionEvent backendRef pos
        = EventMotion <$> convertPoint backendRef pos

convertPoint ::
        forall a . Backend a
        => IORef a
        -> (Int, Int)
        -> IO (Float,Float)
convertPoint backendRef pos
 = do   (sizeX_, sizeY_)        <- getWindowDimensions backendRef
        let (sizeX, sizeY)      = (fromIntegral sizeX_, fromIntegral sizeY_)

        let (px_, py_)          = pos
        let px                  = fromIntegral px_
        let py                  = sizeY - fromIntegral py_

        let px'                 = px - sizeX / 2
        let py'                 = py - sizeY / 2
        let pos'                = (px', py')
        return pos'
