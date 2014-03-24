module Paths_countdown (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/owainlewis/Library/Haskell/ghc-7.4.1/lib/countdown-0.1.0.0/bin"
libdir     = "/Users/owainlewis/Library/Haskell/ghc-7.4.1/lib/countdown-0.1.0.0/lib"
datadir    = "/Users/owainlewis/Library/Haskell/ghc-7.4.1/lib/countdown-0.1.0.0/share"
libexecdir = "/Users/owainlewis/Library/Haskell/ghc-7.4.1/lib/countdown-0.1.0.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "countdown_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "countdown_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "countdown_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "countdown_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
