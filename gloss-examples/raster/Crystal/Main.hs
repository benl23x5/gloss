{-# LANGUAGE BangPatterns #-}


-- Quasicrystals demo. 
--  
-- Based on code from:
--   http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html
--
import Graphics.Gloss.Raster.Field
import System.Environment
import System.Exit
import Data.Char

-- Main -----------------------------------------------------------------------
main :: IO ()
main 
 = do   args    <- getArgs
        config  <- parseArgs args defaultConfig

        let display
             = if configFullScreen config
               then FullScreen
               else InWindow "Crystal"
                             (configSizeX config, configSizeY config)
                             (10, 10)

        let scale =  fromIntegral $ configScale config
        animateField display
                (configZoom config, configZoom config)
                (quasicrystal scale (configDegree config))


-- Config ---------------------------------------------------------------------
data Config
        = Config
        { configSizeX           :: Int
        , configSizeY           :: Int
        , configFullScreen      :: Bool
        , configZoom            :: Int
        , configScale           :: Int
        , configDegree          :: Int }
        deriving Show

defaultConfig :: Config
defaultConfig
        = Config
        { configSizeX           = 800 
        , configSizeY           = 600
        , configFullScreen      = False
        , configZoom            = 2
        , configScale           = 30
        , configDegree          = 5 }


parseArgs :: [String] -> Config -> IO Config
parseArgs args config
        | []    <- args
        = return config

        | "-fullscreen" : rest <- args
        = parseArgs rest
        $ config { configFullScreen     = True }

        | "-window" : sizeX : sizeY : rest <- args
        , all isDigit sizeX
        , all isDigit sizeY
        = parseArgs rest
        $ config { configSizeX          = read sizeX
                 , configSizeY          = read sizeY
                 , configFullScreen     = False }

        | "-zoom" : zoom : rest <- args
        , all isDigit zoom
        = parseArgs rest
        $ config { configZoom           = read zoom }

        | "-scale" : scale : rest <- args
        , all isDigit scale
        = parseArgs rest
        $ config { configScale          = read scale }

        | "-degree" : degree : rest <- args
        , all isDigit degree
        = parseArgs rest
        $ config { configDegree         = read degree }

        | otherwise
        = do    printUsage
                exitWith $ ExitFailure 1

printUsage :: IO ()
printUsage
 = putStr $ unlines
        [ "quazicrystal [flags]"
        , "    -fullscreen              Run full screen"
        , "    -window     sizeX sizeY  Run in a window                     (default 800, 600)"
        , "    -zoom       <NAT>        Pixel replication factor            (default 5)"
        , "    -scale      <NAT>        Feature size of visualisation       (default 30)"
        , "    -degree     <NAT>        Number waves to sum for each point  (default 5)" 
        , ""
        , " You'll want to run this with +RTS -N to enable threads" ]


-- Types ----------------------------------------------------------------------
-- | Angle in radians.
type Angle  = Float

-- | Angle offset used for animation.
type Phi    = Float

-- | Number of waves to sum for each pixel.
type Degree = Int

-- | Feature size of visualisation.
type Scale  = Float

-- | Time in seconds since the program started.
type Time   = Float


-- Point ----------------------------------------------------------------------
-- | Compute a single point of the visualisation.
quasicrystal :: Scale -> Degree -> Time -> Point -> Color
quasicrystal !scale !degree !time !p
 = let  -- Scale the time to be the phi value of the animation.
        -- The action seems to slow down at increasing phi values, 
        -- so we increase phi faster as time moves on.
        phi     = 1 + (time ** 1.5) * 0.005

   in   rampColor 
          $ waves degree phi
          $ point scale p


-- | Sum up all the waves at a particular point.
waves :: Degree -> Phi -> Point -> Float
waves !degree !phi !x = wrap $ waver 0 degree
 where
    !th = pi / phi

    waver :: Float -> Int -> Float
    waver !acc !n
     | n == 0    = acc
     | otherwise = waver (acc + wave (fromIntegral n * th) x)
                         (n - 1)
         
    wrap n 
     = let !n_  = truncate n :: Int
           !n'  = n - fromIntegral n_
       in  if odd n_ then 1 - n'
                     else n'


-- | Generate the value for a single wave.
wave :: Angle -> Point -> Float
wave !th = f where
    !cth  = cos th
    !sth  = sin th

    {-# INLINE f #-}
    f (x, y)  = (cos (cth*x + sth*y) + 1) / 2


-- | Convert an image point to a point on our wave plane.
point :: Scale -> Point -> Point
point !scale (x, y) 
        = (x * scale, y * scale)


-- | Color ramp from blue to white.
rampColor :: Float -> Color
rampColor v 
        = rgb' v (0.4 + (v * 0.6)) 1


