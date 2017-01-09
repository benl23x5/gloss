module Paths_gloss_raster (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [1,10,2,4] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/milica/.cabal/bin"
libdir     = "/home/milica/.cabal/lib/x86_64-linux-ghc-7.10.3/gloss-raster-1.10.2.4-AqR3L6wOLDeBRVuXh6N0tY"
datadir    = "/home/milica/.cabal/share/x86_64-linux-ghc-7.10.3/gloss-raster-1.10.2.4"
libexecdir = "/home/milica/.cabal/libexec"
sysconfdir = "/home/milica/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gloss_raster_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gloss_raster_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gloss_raster_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_raster_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_raster_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
