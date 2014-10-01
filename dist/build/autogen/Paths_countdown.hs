module Paths_countdown (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/owainlewis/Library/Haskell/bin"
libdir     = "/Users/owainlewis/Library/Haskell/ghc-7.8.3-x86_64/lib/countdown-0.1.0.0"
datadir    = "/Users/owainlewis/Library/Haskell/share/ghc-7.8.3-x86_64/countdown-0.1.0.0"
libexecdir = "/Users/owainlewis/Library/Haskell/libexec"
sysconfdir = "/Users/owainlewis/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "countdown_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "countdown_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "countdown_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "countdown_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "countdown_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
