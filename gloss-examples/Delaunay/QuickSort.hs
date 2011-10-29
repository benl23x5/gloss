
{-# LANGUAGE BangPatterns, PatternGuards #-}
{-# OPTIONS  -Wall #-}
module QuickSort
	(quickSortBy)
where
import Data.Vector.Unboxed		as V

quickSortBy :: V.Unbox a => (a -> a -> Ordering) -> Vector a -> Vector a
{-# INLINE quickSortBy #-}
quickSortBy ordering vec
  | V.length vec <= 1	= vec
  | otherwise
  = let	pivot	= vec V.! (V.length vec `div` 2)

	isLower	x 
	 = case ordering x pivot of
		LT	-> True
		_	-> False

	isGreater x
	 = case ordering x pivot of
		GT	-> True
		_	-> False

	lower	= V.filter isLower   vec
	higher	= V.filter isGreater vec
		
    in 	  quickSortBy ordering lower 
     V.++ V.singleton pivot 
     V.++ quickSortBy ordering higher
		
	
	
