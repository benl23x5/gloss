
-- Adapted from ANUPlot version by Clem Baker-Finch
module Main where
import World
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random

-- varying prng sequence
main 
 = do 	gen <- getStdGen
	simulate (InWindow "Lifespan" (800, 600) (10, 10))
		 (greyN 0.1) 	 -- background color
		 2               -- number of steps per second
		 (genesis' gen)  -- initial world
		 render          -- function to convert world to a Picture
		 evolve          -- function to step the world one iteration



