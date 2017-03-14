import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import System.Random

-- x, y, dx, dy
type Particle 
        = (Float, Float, Float, Float) 


main :: IO ()
main 
 = do   g <- getStdGen
        (width,height) <- getScreenSize
        let initialstate = generateParticles g width height
        simulate window background fps initialstate render update
 where
        window          = FullScreen
        background      = black
        fps             = 60
        render xs       =  pictures $ map particleImage xs
        update _        = updateParticles
  

-- | Generates particles from StdGen
generateParticles :: StdGen -> Int -> Int -> [Particle]
generateParticles gen widthInt heightInt 
 = map (g . f)  tups
 where
        -- change range [0,1] ->  [-s/2,s/2]
        f = \(x,y) -> (x * width - width / 2, y * height - height / 2) 

        -- add speed of 0
        g = \(x,y) -> (x,y,0,0) 

        -- 200 Random float tuples
        tups            = take 50 $ zip randoms1 randoms2 
        randoms1        = randoms gen1 :: [Float]
        randoms2        = randoms gen2 :: [Float]
        (gen1,gen2)     = split gen
        width           = toEnum widthInt
        height          = toEnum heightInt


-- | Particle to its picture
particleImage :: Particle -> Picture
particleImage (x,y,_,_) 
 = translate x y $ color white $ circleSolid 2


-- | To update particles for next frame
updateParticles :: Float -> [Particle] -> [Particle]
updateParticles dt 
 = (accelerateParticles dt) . (moveParticles dt)


-- | Moves particles based on their speed
moveParticles :: Float -> [Particle] -> [Particle]
moveParticles dt 
 = map (\(x,y,dx,dy) -> (x+dx*dt,y+dy*dt,dx,dy))


-- | Accelerates particles based on gravity
accelerateParticles :: Float -> [Particle] -> [Particle]
accelerateParticles dt ps 
 = map (gravitate ps dt) ps


-- | Given particles to be gravitating to and for how long,
--  updates a single particle's speed 
gravitate :: [Particle] -> Float -> Particle -> Particle
gravitate [] _ p = p
gravitate ((x',y',_,_):ps) dt p@(x,y,dx,dy) 
 =  -- To dodge divByZero or near divByZero anomalies
    if separated x x' && separated y y' 
    then gravitate ps dt p'
    else gravitate ps dt p
        where
          p'    = (x,y,dx+ddx,dy+ddy)
          ddx   = dirx * g
          ddy   = diry * g
          (dirx,diry) = direction (x,y) (x',y')
          g     = gravitation (x,y) (x',y')
            

-- | Normalized vector from one point to another.
direction :: (Float, Float) -> (Float, Float) -> (Float, Float)
direction (x,y) (x',y') 
 = (dx * scale', dy * scale')
 where
        dx      = x' - x
        dy      = y' - y
        scale'  = 1 / sqrt (dx ^ (2 :: Int) + dy ^ (2 :: Int))
    

-- | Checks if floats not too close to each other
separated :: Float -> Float -> Bool
separated x y 
 = 0.001 < abs (x - y)


-- | Gravitational force of one particle to another
gravitation :: (Float,Float) -> (Float,Float) -> Float
gravitation (x,y) (x',y') 
 = g / sqrt (dx ^ (2 :: Int) + dy ^ (2 :: Int))
 where  dx = x' - x
        dy = y' - y
        g = 1

