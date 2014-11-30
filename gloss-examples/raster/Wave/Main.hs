{-# LANGUAGE BangPatterns, MagicHash #-}
import Graphics.Gloss.Raster.Field
import qualified Data.Vector.Unboxed    as U
import GHC.Prim
import GHC.Exts

main :: IO ()
main 
 = let  !n               = 65536

        !sins            = U.fromList [sin ( (i / n) * (2 * pi)) | i <- [0..n]]

        usin :: Float -> Float
        usin x          = U.unsafeIndex sins (to64k x)
        {-# INLINE usin #-}

        ucos :: Float -> Float
        ucos x          = U.unsafeIndex sins (to64k (x + 0.5))
        {-# INLINE ucos #-}

        get :: Float -> Point -> Color
        get !t (x, y)
         = t `seq` 
           let  !x' = abs $ x + 1
                !y' = abs $ y + 1
                !r1 = abs $ 0.5 * (usin (x' + 0.2 * (usin t)) + ucos y')
                !r2 = abs $ 0.5 * (usin y' + usin (0.3 * t))
           in  rgb' r1 0 r2
        {-# INLINE get #-}

   in   animateField 
                (InWindow "Wave" (800, 600) (100, 100))
                (1, 1)
                get


-- | Map a signed floating point value to a 64k range.
--
--   The range [0 .. 1] maps to [0 .. 65535]
to64k :: Float -> Int
to64k f
 = let  !(F# f')        = f * 65536
        w               = int2Word# (float2Int# f')
        w'              = and# w (int2Word# 0x0ffff#)
        i               = word2Int# w'
   in   (I# i)
{-# INLINE to64k #-}        

 
