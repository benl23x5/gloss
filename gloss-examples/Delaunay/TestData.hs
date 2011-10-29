{-# LANGUAGE ScopedTypeVariables, BangPatterns  #-}
module TestData
	( genPointsUniform
	, genPointsDisc
	, genPointsCombo)
where
import qualified Data.Vector.Unboxed.Mutable	as MV
import qualified Data.Vector.Unboxed		as V
import Data.Vector.Unboxed			(Vector)
import qualified Data.Vector.Generic		as G		
import System.Random
import Data.Word
import Random

-- Random points generation
-- IMPORTANT: We use the same seed with the same random generator in all
--            quickhull codes.  The asymptotic work complexity of quickhull
--            is between O (N) and O (N^2) depending on the input.
--            To compare benchmark results, they always need to use the same
--            input.
seed 		= 42742

-- | Some uniformly distributed points
genPointsUniform 
	:: Int			-- ^ number of points
	-> Double		-- ^ minimum coordinate
	-> Double		-- ^ maximum coordinate
	-> Vector (Double, Double)

genPointsUniform n minXY maxXY
 = let	gen		= mkStdGen seed
   in	V.fromList $ toPairs $ take (2*n) $ randomRs (minXY, maxXY) gen

toPairs []        = []
toPairs (x:y:pts) = (x, y) : toPairs pts


-- | Some points distributed as a disc
genPointsDisc 
	:: Int			-- ^ number of points
	-> (Double, Double) 	-- ^ center of disc
	-> Double 		-- ^ radius of disc
	-> Vector (Double, Double)

genPointsDisc n (originX, originY) radiusMax
 = let	

{-	(genRadius, genAngle)		
		= split $ mkStdGen seed
	
	radius	= V.fromList $ take n $ randomRs (0, radiusMax) genRadius
	angle	= V.fromList $ take n $ randomRs (- pi, pi) genAngle
-}
	radius	= V.map (\x -> (fromIntegral x / 100000) * radiusMax)
		$ randomInts n 0 100000 seed

	angle	= V.map (\x -> (fromIntegral x / 100000) * (2 * pi))
		$ randomInts n 0 100000 (seed + 1)

	makeXY r a
	 	= ( originX + r * cos a
	   	  , originY + r * sin a)	

   in	V.zipWith makeXY radius angle	


-- | A point cloud with areas of high an low density
genPointsCombo 
	:: Int 			-- ^ number of points
	-> Vector (Double, Double)

genPointsCombo n
 	=  genPointsDisc (n `div` 5) (250, 250) 200
	V.++ genPointsDisc (n `div` 5) (100, 100) 80 
	V.++ genPointsDisc (n `div` 5) (150, 300) 30 
	V.++ genPointsDisc (n `div` 5) (500, 120) 30 
	V.++ genPointsDisc (n `div` 5) (300, 200) 150


	
