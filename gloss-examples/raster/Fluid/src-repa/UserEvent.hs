module UserEvent
        (userEvent)
where
import Config
import Model                                    as M
import Data.Array.Repa                          as R
import Graphics.Gloss.Interface.Pure.Game       as G


-- | Handle user events for the Gloss `playIO` wrapper.
userEvent :: Config -> Event -> Model -> Model
userEvent config
        (EventKey key keyState mods (x, y))
        model

        -- Add velocity ---------------------------------------------
        | MouseButton G.RightButton     <- key
        , Down                          <- keyState
        , (x',y')                       <- windowToModel config (x,y)
        = model { clickLoc       = Just (x',y')
                , currButton     = M.RightButton }

        -- Accept shift-leftbutton for people with trackpads
        | MouseButton G.LeftButton      <- key
        , Down                          <- keyState
        , Down                          <- shift mods
        , (x',y')                       <- windowToModel config (x,y)
        = model { clickLoc       = Just (x',y')
                , currButton     = M.RightButton }

        | MouseButton G.RightButton     <- key
        , Up                            <- keyState
        , Just (locX, locY)             <- clickLoc model
        , (x',y')                       <- windowToModel config (x,y)
        = model { velocitySource = Just (SourceDensity (Z:.locY:.locX)
                                         (fromIntegral (locX-x'),fromIntegral (locY-y')))
                 , clickLoc       = Nothing
                 , currButton     = M.None }

        -- Add density ----------------------------------------------
        | MouseButton G.LeftButton <- key
        , Down                          <- keyState
        , (x',y')                       <- windowToModel config (x, y)
        = model { densitySource  = Just (SourceDensity (Z:.y':.x') 1)
                , currButton     = M.LeftButton }

        | MouseButton G.LeftButton <- key
        , Up                       <- keyState
        = model { currButton = M.None }

        -- Reset model
        | Char 'r' <- key
        , Down     <- keyState
        = initModel (configInitialDensity config)
                    (configInitialVelocity config)

        -- Toggle velocity display
        | Char 'v' <- key
        , Down     <- keyState
        = model { drawVelocity = not $ drawVelocity model }

        -- Quit program
        | Char 'q' <- key
        , Down     <- keyState
        = error "Quitting"


userEvent config (EventMotion (x, y)) model
        | M.LeftButton  <- currButton model
        , (x',y')       <- windowToModel config (x, y)
        = model { densitySource  = Just (SourceDensity (Z:.y':.x') 1)
                , currButton     = M.LeftButton
                }

userEvent config (EventMotion (x, y)) model
        | (x', y')              <- windowToModel config (x,y)
        , Just (clx, cly)       <- clickLoc model
        , M.RightButton         <- currButton model
        = model { velocitySource = Just (SourceDensity (Z:.y':.x')
                                        (fromIntegral (clx-x'), fromIntegral (cly-y')))
                , clickLoc       = Just (x',y')
                , currButton     = M.RightButton
                }

userEvent _ _ m = m

-- Converts a window location to the corresponding location in the
-- simulation.
windowToModel :: Config -> (Float, Float) -> (Int, Int)
windowToModel config (x, y) = (x', y')
 where  (scaleX, scaleY)                = configScale config
        (windowWidth, windowHeight)     = configWindowSize config

        x' = round ((x + (fromIntegral windowWidth  / 2)) / fromIntegral scaleX)
        y' = round ((y + (fromIntegral windowHeight / 2)) / fromIntegral scaleY)






