{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Field

main :: IO ()
main 
 = let  get :: Float -> Point -> Color
        get !t _ = makeColor t t t 1.0
        {-# INLINE get #-}
        
   in   animateField 
                (InWindow "Pulse" (800, 600) (100, 100))
                (1, 1) get
