module Paths_probe_rhythm_stimuli (
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

bindir     = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-stimuli/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/bin"
libdir     = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-stimuli/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/lib/x86_64-osx-ghc-7.10.3/probe-rhythm-stimuli-0.1.0.0-GhpfxM5TCHW90UKWP1Xo7K"
datadir    = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-stimuli/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/share/x86_64-osx-ghc-7.10.3/probe-rhythm-stimuli-0.1.0.0"
libexecdir = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-stimuli/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/libexec"
sysconfdir = "/Users/michalhadrava/Code/Probe-rhythm study/probe-rhythm-stimuli/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "probe_rhythm_stimuli_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "probe_rhythm_stimuli_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "probe_rhythm_stimuli_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "probe_rhythm_stimuli_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "probe_rhythm_stimuli_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
