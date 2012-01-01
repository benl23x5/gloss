
import Solver
import System.Environment


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []     -> run 720 450 2 400

         [sizeX, sizeY, zoom, fov]
                -> run (read sizeX) (read sizeY) (read zoom) (read fov)

         _ -> putStr $ unlines
           [ "trace <sizeX::Int> <sizeY::Int> <zoom::Int> (fov::Int)"
           , "    sizeX, sizeY - visualisation size        (default 720, 450)"
           , "    zoom         - pixel replication factor  (default 2)"
           , "    fov          - field of view             (default 400)"
           , ""
           , " You'll want to run this with +RTS -N to enable threads" ]
   
