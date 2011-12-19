
-- Quasicrystals demo. 
--  
-- Based on code from:
--   http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html
--
{-# LANGUAGE BangPatterns #-}
import qualified Data.Array.Repa                        as R
import Graphics.Gloss
import Graphics.Gloss.Field
import System.Environment

-- Types ----------------------------------------------------------------------
-- | Angle in radians.
type Angle  = Float

-- | Angle offset used for animation.
type Phi    = Float

-- | Number of waves to sum for each pixel.
type Degree = Int

-- | Feature size of visualisation.
type Scale  = Float

-- | Size of image to render.
type Size   = Int

-- | How many times to duplicate each pixel / image zoom.
type Zoom   = Int

type Time   = Float


-- Point ----------------------------------------------------------------------
-- | Compute a single point of the visualisation.
quasicrystal :: Size -> Scale -> Degree -> Time -> Point -> Color
quasicrystal !size !scale !degree !time !p
 = let  -- Scale the time to be the phi value of the animation.
        -- The action seems to slow down at increasing phi values, 
        -- so we increase phi faster as time moves on.
        phi     = 1 + (time ** 1.5) * 0.005

   in   rampColor 
          $ waves degree phi
          $ point size scale p


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
point :: Size -> Scale -> Point -> Point
point !size !scale (x, y) = (x * scale, y * scale)


-- | Color ramp from blue to white.
rampColor :: Float -> Color
rampColor v
 = rawColor v (0.4 + (v * 0.6)) 1 1


-- Main -----------------------------------------------------------------------
main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []     -> run 200 3 30 5

         [size, zoom, scale, degree]
                -> run (read size) (read zoom) (read scale) (read degree)

         _ -> putStr $ unlines
           [ "quazicrystal <size::Int> <zoom::Int> <scale::Float> <degree::Int>"
           , "    size    - visualisation size                  (default 200)"
           , "    zoom    - pixel replication factor            (default 3)"
           , "    scale   - feature size of visualisation       (default 30)"
           , "    degree  - number waves to sum for each point  (default 5)" 
           , ""
           , " You'll want to run this with +RTS -N to enable threads" ]
   

run :: Size -> Int -> Scale -> Degree -> IO ()                     
run size zoom scale degree
 = animateFieldInWindow
        "crystal"
        (size, size)
        (10, 10)
        (zoom, zoom)
        (quasicrystal size scale degree)

