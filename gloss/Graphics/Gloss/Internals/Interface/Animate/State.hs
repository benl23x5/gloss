{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Interface.Animate.State
        ( State (..)
        , stateInit )
where

-- | Animation State
data State
        = State
        {
        -- | Whether the animation is running.
          stateAnimate                  :: !Bool

        -- | How many times we've entered the animation loop.
        , stateAnimateCount             :: !Integer

        -- | Whether this is the first frame of the animation.
        , stateAnimateStart             :: !Bool

        -- | Number of msec the animation has been running for
        , stateAnimateTime              :: !Double

        -- | The time when we entered the display callback for the current frame.
        , stateDisplayTime              :: !Double
        , stateDisplayTimeLast          :: !Double

        -- | Clamp the minimum time between frames to this value (in seconds)
        --      Setting this to < 10ms probably isn't worthwhile.
        , stateDisplayTimeClamp         :: !Double

        -- | The time when the last call to the users render function finished.
        , stateGateTimeStart            :: !Double

        -- | The time when displayInWindow last finished (after sleeping to clamp fps).
        , stateGateTimeEnd              :: !Double

        -- | How long it took to draw this frame
        , stateGateTimeElapsed          :: !Double }


stateInit :: State
stateInit
        = State
        { stateAnimate                  = True
        , stateAnimateCount             = 0
        , stateAnimateStart             = True
        , stateAnimateTime              = 0
        , stateDisplayTime              = 0
        , stateDisplayTimeLast          = 0
        , stateDisplayTimeClamp         = 0.01
        , stateGateTimeStart            = 0
        , stateGateTimeEnd              = 0
        , stateGateTimeElapsed          = 0 }
