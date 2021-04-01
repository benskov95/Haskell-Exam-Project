{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_haskell_exam_project (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\mozuh\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\mozuh\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\haskell-exam-project-0.0.0-inplace"
dynlibdir  = "C:\\Users\\mozuh\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\mozuh\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\haskell-exam-project-0.0.0"
libexecdir = "C:\\Users\\mozuh\\AppData\\Roaming\\cabal\\haskell-exam-project-0.0.0-inplace\\x86_64-windows-ghc-9.0.1\\haskell-exam-project-0.0.0"
sysconfdir = "C:\\Users\\mozuh\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_exam_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_exam_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_exam_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_exam_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_exam_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_exam_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
