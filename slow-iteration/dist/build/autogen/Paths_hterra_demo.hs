module Paths_hterra_demo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jeanne/.cabal/bin"
libdir     = "/home/jeanne/.cabal/lib/hterra-demo-0.1.0.0/ghc-7.6.2"
datadir    = "/home/jeanne/.cabal/share/hterra-demo-0.1.0.0"
libexecdir = "/home/jeanne/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hterra_demo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hterra_demo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hterra_demo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hterra_demo_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
