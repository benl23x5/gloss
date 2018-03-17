{-# LANGUAGE RankNTypes #-}

-- We export this stuff separately so we don't clutter up the
-- API of the Graphics.Gloss module.

-- | Simulate mode is for producing an animation of some model who's picture
--   changes over finite time steps. The behavior of the model can also depent
--   on the current `ViewPort`.
module Graphics.Gloss.Interface.IO.Simulate
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , simulateIO
        , ViewPort(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Internals.Interface.Simulate
import Graphics.Gloss.Internals.Interface.Backend


simulateIO :: forall model
        .  Display               -- ^ Display mode.
        -> Color                 -- ^ Background color.
        -> Int                   -- ^ Number of simulation steps to take for each second of real time.
        -> model                 -- ^ The initial model.
        -> (model -> IO Picture) -- ^ A function to convert the model to a picture.
        -> (ViewPort -> Float -> model -> IO model)
                                 -- ^ A function to step the model one iteration. It is passed the
                                 --     current viewport and the amount of time for this simulation
                                 --     step (in seconds).
        -> IO ()

simulateIO = simulateWithBackendIO defaultBackendState
