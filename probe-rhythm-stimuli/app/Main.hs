{-# LANGUAGE OverloadedStrings #-}
module Main where

import Formatting as Fmt
import Control.Monad (zipWithM_,)
import Data.Array.Repa as Arr
import Data.Csv (encode)
import Data.ByteString.Lazy (hPut)
import System.Random (StdGen, getStdGen, randomRs)
import System.IO (withFile, IOMode (WriteMode))
import Temporal.Music as Mus
import Csound.Base as Cs

import Lib

main :: IO ()
main = do
  let totDur = show . sumAllS $ Arr.map Mus.dur stimulusChords
  putStrLn $ formatToString ("Total duration of stimuli: " % string % " s") totDur
  zipWithM_ writeSnd (toList stimulusFileNames) (toList stimuli)
  g <- getStdGen
  writeSnd "calibration.wav" $ calibrationStimulus g
  let periods = encode $ toList fareyStimulusPeriods
  withFile "periods.csv" WriteMode (`hPut` periods)
  return ()
