{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( stimulusChords
    , stimulusFileNames
    , stimuli
    , calibrationStimulus
    ) where

import Prelude as P
import Formatting as Fmt
import System.Random (StdGen, getStdGen, randomRs)

import Data.Array.Repa as Arr
import Data.Array.Repa.Repr.Vector (V,)
import Data.Array.Repa.Eval (fromList,)

import Temporal.Music as Mus
import Temporal.Music.Scales as Sc

import Csound.Base as Cs
import Csound.Catalog.Drum.Tr808 as Cat

-- | Convert 'Drum' to 'CsdNote' of the same volume and unit frequency.
csdDrum :: Drum a -> CsdNote Cs.D
csdDrum d = (double . absVolume $ drumVolume d, 1)

scoreToSco :: Score a -> Sco a
scoreToSco = Cs.har . fmap f . Mus.render
    where f (Event s d a) = Cs.del (double s) . Cs.str (double d) $ Cs.temp a

csdDrums :: Score (Drum a) -> Sco (CsdNote Cs.D)
csdDrums = fmap csdDrum . scoreToSco

-- | Chromatic scale in twelve-tone equal temperament and a special just tuning.
scaleNames :: Array V DIM1 String
scaleNames = fromList (Z :. 2) ["12TET", "12TFarey"]

-- | Given an absolute frequency in Hz, these generators return a chromatic scale.
scaleGenerators :: Array V DIM1 (Hz -> Scale)
scaleGenerators = fromList (Z :. 2) [eqt, farey]
    where farey = fromIntervals 2 [1, 16/15, 9/8, 6/5, 5/4, 4/3, 17/12, 3/2, 8/5, 5/3, 16/9, 15/8]

-- | Absolute frequencies of tonics for the two chromatic scales.
scaleBases :: Array V DIM1 Hz
scaleBases = fromList (Z :. 4) $ P.map (scaleAt (eqt 1)) [0, 4, 8, 12]

-- | Chromatic scale for each combination of tuning and absolute tonic frequency.
scales :: Array Arr.D DIM2 Scale
scales = traverse2 scaleGenerators scaleBases fs fe
    where fs (Z :. ng) (Z :. nb) = Z :. ng :. nb
          fe g b (Z :. ig :. ib) = g (Z :. ig) $ b (Z :. ib)

-- | Major tonic triad for each of the chromatic scales.
contextNotes :: Array Arr.D DIM2 [Score (Note a)]
contextNotes = Arr.map f scales
    where f s = P.map (setScale s . nx) [0, 4, 7]

{-|
    Each context chord will be presented either alone or simultaneously
    with a probe note corresponding to a chromatic step.
-}
probeSteps :: Array V DIM1 (Maybe Step)
probeSteps = fromList (Z :. 10) $ Nothing : P.map Just [1, 2, 3, 5, 6, 8, 9, 10, 11]

-- | Probe notes corresponding to the chromatic steps above taken from a given scale.
probeNotes :: Array Arr.D DIM3 (Maybe (Score (Note a)))
probeNotes = traverse2 scales probeSteps fs fe
    where fs (Z :. ng :. nb) (Z :. nst) = Z :. ng :. nb :. nst
          fe sc st (Z :. ig :. ib :. ist) = (setScale (sc (Z :. ig :. ib)) . nx) <$> st (Z :. ist)

-- | Tonic triads, possibly with a probe note added - they correspond to the stimuli used in our experiment.
stimulusChords :: Array Arr.D DIM3 (Score (Note a))
stimulusChords = Arr.zipWith f contextNotes' probeNotes
    where f cns (Just pn) = Mus.har (pn:cns)
          f cns Nothing   = Mus.har cns
          contextNotes' = extend (Z :. Arr.All :. Arr.All :. (10::Int)) contextNotes

-- | Names for the sound files to be used as stimuli.
stimulusFileNames :: Array Arr.D DIM3 String
stimulusFileNames = traverse3 scaleNames scaleBases probeSteps fs fe
    where fs (Z :. ng) (Z :. nb) (Z :. nst) = Z :. ng :. nb :. nst
          fe n b st (Z :. ig :. ib :. ist) = case st (Z :. ist) of
              Just step -> formatToString (string % "-" % fixed 3 % "-" % Fmt.int % ".wav") (n (Z :. ig)) (b (Z :. ib)) step
              Nothing   -> formatToString (string % "-" % fixed 3 % ".wav") (n (Z :. ig)) (b (Z :. ib))

-- | Render a note as a rhythm with bpm equal to the frequency of the note.
noteToRhythm :: Event t (Note a) -> Score (Drum a)
noteToRhythm e = loopBy (floor $ r * 16) . Mus.str (1 / f) . setLevel 5 $ Mus.temp def
    where p = notePitch $ eventContent e
          f = absPitch p
          r = scaleStep (pitchScale p) $ pitchStep p

-- | Stimulus chords rendered as polyrhythms.
stimulusRhythms :: Array Arr.D DIM3 (Score (Drum a))
stimulusRhythms = Arr.map f stimulusChords
    where f = foldl1 (Mus.=:=) . Mus.tmap noteToRhythm

-- | A patch for rendering the polyrhythms.
patch :: Patch2
patch = Patch instr []
    where instr (a, _) = do let a' = sig a
                            ch1 <- Cat.rimShot
                            ch2 <- Cat.rimShot
                            return (a' * ch1, a' * ch2)

-- | Finally, the stimuli to be written to disk.
stimuli :: Array Arr.D DIM3 Sig2
stimuli = Arr.map (mix . atSco patch . csdDrums) stimulusRhythms

-- | Random rhythm for calibration of audio equipment before the sessions.
calibrationRhythm :: StdGen -> Score (Drum a)
calibrationRhythm g = Mus.mel [Mus.str (1 / f) . setLevel 5 $ Mus.temp def | f <- fs]
    where fs = take 40 . P.map (scaleAt (eqt 1)) $ randomRs (0, 23) g

-- | Calibration stimulus.
calibrationStimulus :: StdGen -> Sig2
calibrationStimulus = mix . atSco patch . csdDrums . calibrationRhythm
