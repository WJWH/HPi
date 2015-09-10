module Paths_HPi (
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
version = Version {versionBranch = [0,4,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\HPi-0.4.0"
datadir    = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\HPi-0.4.0"
libexecdir = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\HPi-0.4.0"
sysconfdir = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HPi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HPi_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HPi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HPi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HPi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
