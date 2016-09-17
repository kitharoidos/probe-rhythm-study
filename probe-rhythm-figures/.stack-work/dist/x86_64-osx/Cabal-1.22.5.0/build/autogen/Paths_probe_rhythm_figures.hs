module Paths_probe_rhythm_figures (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-figures/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/bin"
libdir     = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-figures/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/lib/x86_64-osx-ghc-7.10.3/probe-rhythm-figures-0.1.0.0-G2rIiywLUzz2z377ALaJVY"
datadir    = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-figures/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/share/x86_64-osx-ghc-7.10.3/probe-rhythm-figures-0.1.0.0"
libexecdir = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-figures/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/libexec"
sysconfdir = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-figures/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "probe_rhythm_figures_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "probe_rhythm_figures_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "probe_rhythm_figures_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "probe_rhythm_figures_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "probe_rhythm_figures_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
