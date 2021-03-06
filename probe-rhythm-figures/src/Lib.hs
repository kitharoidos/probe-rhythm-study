{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
    ( vdist
    , hdist
    , beside'
    , tones
    , rhythms
    , chord
    , polyRhythm
    , rhythmsWithSync
    , labelOscs
    , labelPolyOsc
    , oscScale
    , farey
    , eqt
    ) where

import Prelude as P
import Control.Arrow (first, second)
import Diagrams.Prelude as D
import Diagrams.Trail (trailPoints)
import Diagrams.Backend.SVG.CmdLine
import Numeric.LinearAlgebra.Data as N
import Data.List.Split (chunksOf)
import Data.List as L
import Data.Function (on)

-- | Distribute diagrams.
dist :: (CatOpts Double -> [Diagram B] -> Diagram B) -> Double -> [Diagram B] -> Diagram B
dist distFn sepa = distFn (with & catMethod .~ Distrib & sep .~ sepa)

-- | Distribute diagrams vertically.
vdist :: Double -> [Diagram B] -> Diagram B
vdist = dist vcat'

-- | Distribute diagrams horizontally.
hdist :: Double -> [Diagram B] -> Diagram B
hdist = dist hcat'

beside' = flip . beside . negated

-- | Frame diagram using a rectangle of given height.
frameH :: Double -> Diagram B -> Diagram B
frameH h d = d # centerXY `atop` rect (width d) h # lw none

-- | Half-sinusoid approximated with a bezier curve.
toneHalfCycle :: Trail V2 Double
toneHalfCycle = trailFromSegments [bezier3 (0.5 D.*^ unitX) (r2 (0.5, -1)) (r2 (1, -1))]

-- | Half-tick.
rhythmHalfCycle :: Trail V2 Double
rhythmHalfCycle = trailFromSegments [straight (-unitY), straight unitX]

-- | Make full cycle by sticking together a half-cycle and its reflection around the y axis.
oscCycle :: Trail V2 Double -> Trail V2 Double
oscCycle halfCycle = halfCycle <> reflectY halfCycle

-- | One cycle of sinusoid approximated with two bezier curves.
toneCycle :: Trail V2 Double
toneCycle = oscCycle toneHalfCycle

-- | Tick.
rhythmCycle :: Trail V2 Double
rhythmCycle = oscCycle rhythmHalfCycle

-- | From a given cycle, take the segment from p0 to p1 and replicate it w times.
osc :: Trail V2 Double -> (Double, Double) -> Double -> Trail V2 Double
osc cyc (p0, p1) w = foldl1 (<>) $ replicate n cyc P.++ app
  where n   = floor w
        w'  = w - fromIntegral n
        app = [section cyc 0 (p0 + w' * (p1 - p0))]

-- | Sinusoid approximated with bezier curves.
tone :: Double -> Trail V2 Double
tone = osc toneCycle (0, 1)

-- | Tick train.
rhythm :: Double -> Trail V2 Double
rhythm = osc rhythmCycle (0.25, 0.75)

-- | Multi-frequency oscillation.
polyOsc :: (Double -> Trail V2 Double) -> (Double -> [(Colour Double, Trail V2 Double)] -> Diagram B) -> Double -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
polyOsc oscFn combFn sepa w0 pcs tn f0 = combFn sepa oscs # scaleX (1 / f0')
  where fs   = P.map (second (\pc -> tn ! pc)) $ sortOn (negate . snd) pcs
        oscs = P.map (second (\f -> oscFn (w0' * f) # scaleX (1 / f))) fs
        w0'  = fromIntegral w0
        f0'  = fromIntegral f0

-- | Oscillations.
oscs :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
oscs strokeFn sepa = vdist sepa . P.map (\(col, osc) -> strokeFn osc # lc col)

-- | Compound waveform.
chord' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
chord' _ _ oscs = (cubicSpline False chdSmps :: Trail V2 Double) # stroke # frameH nOscs
  where oscs'   = snd $ unzip oscs
        nOscs   = fromIntegral $ length oscs
        maxLen  = length . trailSegments $ head oscs'
        par     = toList $ linspace (4 * maxLen) (0, 1)
        oscSmps = L.transpose [[osc `atParam` p | p <- par] | osc <- P.map (`at` origin) oscs']
        chdSmps = P.map (foldl (\pt pt' -> (pt' ^. _x) ^& (pt ^. _y + pt' ^. _y)) origin) oscSmps

-- | Compound tick train.
polyRhythm' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
polyRhythm' strokeFn _ oscs = frameH nOscs . mconcat . P.map strokeFn . snd $ unzip oscs
  where nOscs = fromIntegral $ length oscs

-- | Curve marking a point of approximate synchronization.
syncCurve :: V2 Double -> Trail V2 Double
syncCurve vect = trailFromSegments [bezier3 (0.5 * y' D.*^ unitY) (r2 (x', 0.5 * y')) vect']
  where vect'    = vect ^+^ unitY
        x' :& y' = coords vect'

-- | Curves marking points of approximate synchronization between adjacent rhythms.
rhythmSync' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
rhythmSync' _ sepa oscs = concat curves # mconcat # translateY (-1)
  where yCoords = P.map (D.*^ unitY) [0, -sepa ..]
        grid    = P.map (init . trailPoints) $ P.zipWith at (snd $ unzip oscs) yCoords
        grid'   = P.map (P.map head . chunksOf 4) grid
        vects   = P.zipWith (\r1 r2 -> [[p2 .-. p1 | p2 <- r2] | p1 <- r1]) grid' (tail grid')
        nrst    = P.map (P.map (minimumBy (compare `on` norm))) vects
        opcts   = P.map (P.map (\v -> cosA (angleBetween v (-unitY)) # (**64))) nrst
        curves  = P.zipWith3 (P.zipWith3 (\v p o -> (syncCurve v `at` p) # strokeLocTrail # opacity o))
                             nrst grid' opcts

strokeTone :: Trail V2 Double -> Diagram B
strokeTone = stroke

strokeRhythm :: Trail V2 Double -> Diagram B
strokeRhythm cyc = hcat $ P.zipWith ($) strokeFns segments
  where segments  = explodeTrail $ cyc `at` origin
        strokeFns = cycle (strokeLocTrail : replicate 3 (strokeLocTrail # lw none))

label :: (Double -> [Diagram B] -> Diagram B) -> Double -> [(Colour Double, Int)] -> Diagram B
label lblFn sepa pcs = lblFn sepa lbl
  where pcs' = sortOn (negate . snd) pcs
        nt   = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
        lbl  = P.map (\(col, pc) -> alignedText 0 0.5 (nt !! pc) # lw none # fc col) pcs'

-- | Label oscillations.
labelOscs :: Double -> [(Colour Double, Int)] -> Diagram B
labelOscs = label vdist

-- | Sinusoids.
tones :: Double -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
tones = polyOsc tone (oscs strokeTone)

-- | Rhythms.
rhythms :: Double -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
rhythms = polyOsc rhythm (oscs strokeRhythm)

-- | Label multi-frequency oscillation.
labelPolyOsc :: Double -> [(Colour Double, Int)] -> Diagram B
labelPolyOsc _ = label vdist 1

-- | Superposition of several sinusoids.
chord :: Double -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
chord = polyOsc tone (chord' strokeTone)

-- | Superposition of several tick trains.
polyRhythm :: Double -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
polyRhythm = polyOsc rhythm (polyRhythm' strokeRhythm)

-- | Approximate points of synchronization between adjacent rhythms.
rhythmSync :: Double -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
rhythmSync = polyOsc rhythm (rhythmSync' strokeRhythm)

-- | Rhythms with approximate points of synchronization highlighted.
rhythmsWithSync :: Double -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
rhythmsWithSync sepa w0 pcs tn f0 = rhythms sepa w0 pcs tn f0 `atop` rhythmSync sepa w0 pcs tn f0

-- | Scale for oscillation plots.
oscScale :: Int -> String -> Diagram B
oscScale f0 txt = vsep 1 [glyph, txt']
  where glyph =   map straight [-0.5 *^ unitY, unitX, 0.5 *^ unitY] # fromSegments
                # strokeTrail # scaleX (2 / fromIntegral f0) # centerXY
        txt'  = text txt # centerXY

-- | Farey tuning.
farey :: Vector Double
farey = fromList [1, 16/15, 9/8, 6/5, 5/4, 4/3, 17/12, 3/2, 8/5, 5/3, 16/9, 15/8]

-- | 12TET tuning.
eqt :: Vector Double
eqt = fromList [2**(i / 12) | i <- [0 .. 11]]
