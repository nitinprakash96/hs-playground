{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hs_playground (
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

bindir     = "/Users/nitin/.cabal/bin"
libdir     = "/Users/nitin/.cabal/lib/x86_64-osx-ghc-8.8.4/hs-playground-0.1.0.0-inplace-hs-playground"
dynlibdir  = "/Users/nitin/.cabal/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/nitin/.cabal/share/x86_64-osx-ghc-8.8.4/hs-playground-0.1.0.0"
libexecdir = "/Users/nitin/.cabal/libexec/x86_64-osx-ghc-8.8.4/hs-playground-0.1.0.0"
sysconfdir = "/Users/nitin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hs_playground_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hs_playground_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hs_playground_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hs_playground_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hs_playground_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hs_playground_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
