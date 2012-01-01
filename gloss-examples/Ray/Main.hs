
import Solver
import qualified Graphics.Gloss         as G
import qualified Graphics.Gloss.Field   as G
import System.Environment

-- Main -----------------------------------------------------------------------
main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []     -> run 800 300 2

         [sizeX, sizeY, zoom]
                -> run (read sizeX) (read sizeY) (read zoom)

         _ -> putStr $ unlines
           [ "trace <sizeX::Int> <sizeY::Int> <zoom::Int>"
           , "    size    - visualisation size                  (default 200)"
           , "    zoom    - pixel replication factor            (default 2)"
           , ""
           , " You'll want to run this with +RTS -N to enable threads" ]
   
