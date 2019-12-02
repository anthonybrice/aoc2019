{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aoc2019 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/abrice/repos/aoc2019/.stack-work/install/x86_64-osx/e18b6062ad717646229464e1232ff1e2d5d8dc1e70f6311750ab90ac7c89a51a/8.6.5/bin"
libdir     = "/Users/abrice/repos/aoc2019/.stack-work/install/x86_64-osx/e18b6062ad717646229464e1232ff1e2d5d8dc1e70f6311750ab90ac7c89a51a/8.6.5/lib/x86_64-osx-ghc-8.6.5/aoc2019-0.1.0.0-6Zq7njaL4e4CaDbDIYBaEx-aoc2019"
dynlibdir  = "/Users/abrice/repos/aoc2019/.stack-work/install/x86_64-osx/e18b6062ad717646229464e1232ff1e2d5d8dc1e70f6311750ab90ac7c89a51a/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/abrice/repos/aoc2019/.stack-work/install/x86_64-osx/e18b6062ad717646229464e1232ff1e2d5d8dc1e70f6311750ab90ac7c89a51a/8.6.5/share/x86_64-osx-ghc-8.6.5/aoc2019-0.1.0.0"
libexecdir = "/Users/abrice/repos/aoc2019/.stack-work/install/x86_64-osx/e18b6062ad717646229464e1232ff1e2d5d8dc1e70f6311750ab90ac7c89a51a/8.6.5/libexec/x86_64-osx-ghc-8.6.5/aoc2019-0.1.0.0"
sysconfdir = "/Users/abrice/repos/aoc2019/.stack-work/install/x86_64-osx/e18b6062ad717646229464e1232ff1e2d5d8dc1e70f6311750ab90ac7c89a51a/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aoc2019_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aoc2019_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aoc2019_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aoc2019_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc2019_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc2019_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
