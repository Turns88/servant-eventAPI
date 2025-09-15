{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_servant_eventAPI (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/paul/Projects/Github/servant-eventAPI/servant-eventAPI/.stack-work/install/x86_64-linux/918fbdf135b04c3bb9495fe348df373bb7fbf53cf9c910ceeea934e81f56ffed/9.10.2/bin"
libdir     = "/home/paul/Projects/Github/servant-eventAPI/servant-eventAPI/.stack-work/install/x86_64-linux/918fbdf135b04c3bb9495fe348df373bb7fbf53cf9c910ceeea934e81f56ffed/9.10.2/lib/x86_64-linux-ghc-9.10.2-aac9/servant-eventAPI-0.1.0.0-6hJZsxyIPVn5GIgtK6HlLY-servant-eventAPI-exe"
dynlibdir  = "/home/paul/Projects/Github/servant-eventAPI/servant-eventAPI/.stack-work/install/x86_64-linux/918fbdf135b04c3bb9495fe348df373bb7fbf53cf9c910ceeea934e81f56ffed/9.10.2/lib/x86_64-linux-ghc-9.10.2-aac9"
datadir    = "/home/paul/Projects/Github/servant-eventAPI/servant-eventAPI/.stack-work/install/x86_64-linux/918fbdf135b04c3bb9495fe348df373bb7fbf53cf9c910ceeea934e81f56ffed/9.10.2/share/x86_64-linux-ghc-9.10.2-aac9/servant-eventAPI-0.1.0.0"
libexecdir = "/home/paul/Projects/Github/servant-eventAPI/servant-eventAPI/.stack-work/install/x86_64-linux/918fbdf135b04c3bb9495fe348df373bb7fbf53cf9c910ceeea934e81f56ffed/9.10.2/libexec/x86_64-linux-ghc-9.10.2-aac9/servant-eventAPI-0.1.0.0"
sysconfdir = "/home/paul/Projects/Github/servant-eventAPI/servant-eventAPI/.stack-work/install/x86_64-linux/918fbdf135b04c3bb9495fe348df373bb7fbf53cf9c910ceeea934e81f56ffed/9.10.2/etc"

getBinDir     = catchIO (getEnv "servant_eventAPI_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "servant_eventAPI_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "servant_eventAPI_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "servant_eventAPI_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "servant_eventAPI_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "servant_eventAPI_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
