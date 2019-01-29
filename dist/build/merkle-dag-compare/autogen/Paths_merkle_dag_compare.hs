{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_merkle_dag_compare (
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

bindir     = "/home/pk/dev/merkle-dag-compare/.cabal-sandbox/bin"
libdir     = "/home/pk/dev/merkle-dag-compare/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/merkle-dag-compare-0.1.0.0-5K8hcXtVsBU4SP1jddyqks-merkle-dag-compare"
dynlibdir  = "/home/pk/dev/merkle-dag-compare/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3"
datadir    = "/home/pk/dev/merkle-dag-compare/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/merkle-dag-compare-0.1.0.0"
libexecdir = "/home/pk/dev/merkle-dag-compare/.cabal-sandbox/libexec/x86_64-linux-ghc-7.10.3/merkle-dag-compare-0.1.0.0"
sysconfdir = "/home/pk/dev/merkle-dag-compare/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "merkle_dag_compare_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "merkle_dag_compare_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "merkle_dag_compare_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "merkle_dag_compare_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "merkle_dag_compare_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "merkle_dag_compare_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
