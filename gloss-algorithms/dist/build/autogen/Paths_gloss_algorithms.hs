module Paths_gloss_algorithms (
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
version = Version [1,10,2,3] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/milica/.cabal/bin"
libdir     = "/home/milica/.cabal/lib/x86_64-linux-ghc-7.10.3/gloss-algorithms-1.10.2.3-CFBHe9GvJlx1QAioqJFmi1"
datadir    = "/home/milica/.cabal/share/x86_64-linux-ghc-7.10.3/gloss-algorithms-1.10.2.3"
libexecdir = "/home/milica/.cabal/libexec"
sysconfdir = "/home/milica/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gloss_algorithms_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gloss_algorithms_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gloss_algorithms_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_algorithms_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_algorithms_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
