{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
    ( tones
    , rhythms
    , chord
    , polyRhythm
    , rhythmsWithSync
    , farey
    , eqt
    ) where

import Prelude as P
import Control.Arrow (first, second)
import Diagrams.Prelude as D
import Diagrams.Trail (trailPoints)
import Diagrams.Backend.SVG.CmdLine
import Data.Array.Repa as Arr
import Data.List.Split (chunksOf)
import Data.List (sortOn, minimumBy)
import Data.Function (on)

toneHalfCycle :: Trail V2 Double
toneHalfCycle = trailFromSegments [bezier3 (0.5 D.*^ unitX) (r2 (0.5, -1)) (r2 (1, -1))]

rhythmHalfCycle :: Trail V2 Double
rhythmHalfCycle = trailFromSegments
  [straight (-0.5 D.*^ unitY), straight unitX, straight (-0.5 D.*^ unitY)]

oscCycle :: Trail V2 Double -> Trail V2 Double
oscCycle halfCycle = halfCycle <> reflectY halfCycle

toneCycle :: Trail V2 Double
toneCycle = oscCycle toneHalfCycle

rhythmCycle :: Trail V2 Double
rhythmCycle = oscCycle rhythmHalfCycle

osc :: Trail V2 Double -> Double -> Trail V2 Double
osc cyc w = foldl1 (<>) $ replicate n cyc P.++ [section cyc 0 w']
  where n  = floor w
        w' = w - fromIntegral n

tone :: Double -> Trail V2 Double
tone = osc toneCycle

rhythm :: Double -> Trail V2 Double
rhythm = osc rhythmCycle

polyOsc :: (Double -> Trail V2 Double) -> (Double -> [(Colour Double, Trail V2 Double)] -> Diagram B) -> Double -> [(Colour Double, Int)] -> Array U DIM1 Double -> Double -> Diagram B
polyOsc oscFn combFn w0 pcs tn f0 = combFn sepa oscs # scaleX (1 / f0)
  where fs   = P.map (second (\pc -> tn ! (Z :. pc))) $ sortOn (negate . snd) pcs
        oscs = P.map (second (\f -> oscFn (w0 * f) # scaleX (1 / f))) fs
        sepa = 2

oscs :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
oscs strokeFn sepa =   vcat' (with & catMethod .~ Distrib & sep .~ sepa)
                     . P.map (\(col, osc) -> strokeFn osc # lc col)

chord' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
chord' _ _ oscs = (cubicSpline False pts :: Trail V2 Double) # stroke
  where oscs'   = snd $ unzip oscs
        maxLen  = maximum $ P.map (length . trailSegments) oscs'
        par     = [0, 1 / (1e2 * 0.5 * fromIntegral maxLen) .. 1]
        pts     = P.map (\p -> sum $ P.map ((`atParam` p) . (`at` origin)) oscs') par

polyRhythm' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
polyRhythm' strokeFn _ oscs = mconcat . P.map strokeFn . snd $ unzip oscs

syncCurve :: V2 Double -> Trail V2 Double
syncCurve vect = trailFromSegments [bezier3 (0.5 * y' D.*^ unitY) (r2 (x', 0.5 * y')) vect']
  where vect'    = vect ^+^ 0.5 D.*^ unitY
        x' :& y' = coords vect'

rhythmSync' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
rhythmSync' _ sepa oscs = concat curves # P.map strokeLocTrail # mconcat # translateY (-0.5) # opacity 0.5
  where yCoords = P.map (D.*^ unitY) [0, -sepa ..]
        grid    = P.map (init . trailPoints) $ P.zipWith at (snd $ unzip oscs) yCoords
        grid'   = P.map (P.map head . chunksOf 6) grid
        vects   = P.zipWith (\r1 r2 -> [[p2 .-. p1 | p2 <- r2] | p1 <- r1]) grid' (tail grid')
        nrst    = P.map (P.map (minimumBy (compare `on` norm))) vects
        curves  = P.zipWith (P.zipWith (\v p -> syncCurve v `at` p)) nrst grid'

strokeTone :: Trail V2 Double -> Diagram B
strokeTone = stroke

strokeRhythm :: Trail V2 Double -> Diagram B
strokeRhythm cyc = hcat $ P.zipWith ($) strokeFns segments
  where segments  = explodeTrail $ cyc `at` origin
        strokeFns = cycle (strokeLocTrail : replicate 5 (strokeLocTrail # lw none))

tones :: Double -> [(Colour Double, Int)] -> Array U DIM1 Double -> Double -> Diagram B
tones = polyOsc tone (oscs strokeTone)

rhythms :: Double -> [(Colour Double, Int)] -> Array U DIM1 Double -> Double -> Diagram B
rhythms = polyOsc rhythm (oscs strokeRhythm)

chord :: Double -> [(Colour Double, Int)] -> Array U DIM1 Double -> Double -> Diagram B
chord = polyOsc tone (chord' strokeTone)

polyRhythm :: Double -> [(Colour Double, Int)] -> Array U DIM1 Double -> Double -> Diagram B
polyRhythm = polyOsc rhythm (polyRhythm' strokeRhythm)

rhythmSync :: Double -> [(Colour Double, Int)] -> Array U DIM1 Double -> Double -> Diagram B
rhythmSync = polyOsc rhythm (rhythmSync' strokeRhythm)

rhythmsWithSync :: Double -> [(Colour Double, Int)] -> Array U DIM1 Double -> Double -> Diagram B
rhythmsWithSync w0 pcs tn f0 = rhythms w0 pcs tn f0 `atop` rhythmSync w0 pcs tn f0

farey :: Array U DIM1 Double
farey = fromListUnboxed (Z :. 12) [1, 16/15, 9/8, 6/5, 5/4, 4/3, 17/12, 3/2, 8/5, 5/3, 16/9, 15/8]

eqt :: Array U DIM1 Double
eqt = fromListUnboxed (Z :. 12) [2**(i / 12) | i <- [0 .. 11]]
