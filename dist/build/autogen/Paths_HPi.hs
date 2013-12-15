module Paths_HPi (
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
version = Version {versionBranch = [0,4,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\HPi-0.4.0\\ghc-7.4.2"
datadir    = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\HPi-0.4.0"
libexecdir = "C:\\Users\\Wander\\AppData\\Roaming\\cabal\\HPi-0.4.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HPi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HPi_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HPi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HPi_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
