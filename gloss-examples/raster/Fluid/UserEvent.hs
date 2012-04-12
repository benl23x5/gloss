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
              (EventKey key keyState _mods (x, y)) 
        model@(Model df ds vf vs cl sp _cb)

        | MouseButton G.LeftButton <- key
        , Down                          <- keyState
        , (x',y')                       <- windowToModel config (x, y) 
        = Model { densityField   = df
                , densitySource  = Just (Source (Z:.y':.x') 1)
                , velocityField  = vf
                , velocitySource = vs
                , clickLoc       = cl
                , stepsPassed    = sp
                , currButton     = M.LeftButton
                }

        | MouseButton G.LeftButton <- key
        , Up                       <- keyState
        = Model { densityField   = df
                , densitySource  = ds
                , velocityField  = vf
                , velocitySource = vs
                , clickLoc       = cl
                , stepsPassed    = sp
                , currButton     = M.None
                }

        | MouseButton G.RightButton     <- key
        , Down                          <- keyState
        , (x',y')                       <- windowToModel config (x,y)
        = Model { densityField   = df
                , densitySource  = ds
                , velocityField  = vf
                , velocitySource = vs
                , clickLoc       = Just (x',y')
                , stepsPassed    = sp
                , currButton     = M.RightButton
                }

        | MouseButton G.RightButton     <- key
        , Up                            <- keyState
        , Just (locX, locY)             <- cl
        , (x',y')                       <- windowToModel config (x,y)
        =  Model { densityField   = df
                 , densitySource  = ds
                 , velocityField  = vf
                 , velocitySource = Just (Source (Z:.locY:.locX)
                                         (fromIntegral (locX-x'),fromIntegral (locY-y')))
                 , clickLoc       = Nothing
                 , stepsPassed    = sp
                 , currButton     = M.None
                 }

        | Char 'r' <- key
        , Down     <- keyState
        = let   (_ :. height :. width) = R.extent (densityField model)
          in    initModel width height

        | Char 'q' <- key
        , Down     <- keyState
        = error "Quitting"


userEvent config (EventMotion (x, y)) 
        (Model df _ds vf vs cl sp M.LeftButton)
        | (x',y')                       <- windowToModel config (x, y) 
        = Model { densityField   = df
                , densitySource  = Just (Source (Z:.y':.x') 1)
                , velocityField  = vf
                , velocitySource = vs
                , clickLoc       = cl
                , stepsPassed    = sp
                , currButton     = M.LeftButton
                }

userEvent config (EventMotion (x, y)) 
          (Model df ds vf _vs (Just (clx, cly)) sp M.RightButton)
        | (x', y')                      <- windowToModel config (x,y)
        = Model { densityField   = df
                , densitySource  = ds
                , velocityField  = vf
                , velocitySource = Just (Source (Z:.y':.x')
                                        (fromIntegral (clx-x'), fromIntegral (cly-y')))
                , clickLoc       = Just (x',y')
                , stepsPassed    = sp
                , currButton     = M.RightButton
                }

userEvent _ _ m = m

-- Converts a window location to the corresponding location in the
-- simulation.
windowToModel :: Config -> (Float, Float) -> (Int, Int)
windowToModel config (x, y) = (x', y')
 where  (scaleX, scaleY)                = configScale config
        (windowWidth, windowHeight)     = configWindowSize config

        x' = round ((x + (fromIntegral windowWidth  / 2)) / scaleX)
        y' = round ((y + (fromIntegral windowHeight / 2)) / scaleY)






