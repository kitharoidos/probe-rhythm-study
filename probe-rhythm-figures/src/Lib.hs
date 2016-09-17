{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
    ( vdist
    , hdist
    , tones
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
import Numeric.LinearAlgebra.Data as N
import Data.List.Split (chunksOf)
import Data.List as L
import Data.Function (on)

dist :: (CatOpts Double -> [Diagram B] -> Diagram B) -> Double -> [Diagram B] -> Diagram B
dist distFn sepa = distFn (with & catMethod .~ Distrib & sep .~ sepa)

vdist :: Double -> [Diagram B] -> Diagram B
vdist = dist vcat'

hdist :: Double -> [Diagram B] -> Diagram B
hdist = dist hcat'

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
osc cyc w = foldl1 (<>) $ replicate n cyc P.++ app
  where n   = floor w
        w'  = w - fromIntegral n
        app = [section cyc 0 w' | w' > 1e-2]

tone :: Double -> Trail V2 Double
tone = osc toneCycle

rhythm :: Double -> Trail V2 Double
rhythm = osc rhythmCycle

polyOsc :: (Double -> Trail V2 Double) -> (Double -> [(Colour Double, Trail V2 Double)] -> Diagram B) -> (Double -> [Diagram B] -> Diagram B) -> Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
polyOsc oscFn combFn lblFn w0 pcs tn f0 = hdist sepa
  [lblFn sepa lbl # translateY (-0.5), combFn sepa oscs # scaleX (1 / f0')]
  where pcs' = sortOn (negate . snd) pcs
        fs   = P.map (second (\pc -> tn ! pc)) pcs'
        oscs = P.map (second (\f -> oscFn (w0' * f) # scaleX (1 / f))) fs
        w0'  = fromIntegral w0
        f0'  = fromIntegral f0
        nt   = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
        lbl  = P.map (\(col, pc) -> alignedText 0 0.5 (nt !! pc) # lw none # fc col) pcs'
        sepa = 2

oscs :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
oscs strokeFn sepa = vdist sepa . P.map (\(col, osc) -> strokeFn osc # lc col)

chord' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
chord' _ _ oscs = (cubicSpline False chdSmps :: Trail V2 Double) # stroke
  where oscs'   = snd $ unzip oscs
        maxLen  = length . trailSegments $ head oscs'
        par     = toList $ linspace (4 * maxLen) (0, 1)
        oscSmps = L.transpose [[osc `atParam` p | p <- par] | osc <- P.map (`at` origin) oscs']
        chdSmps = P.map (foldl (\pt pt' -> (pt' ^. _x) ^& (pt ^. _y + pt' ^. _y)) origin) oscSmps

polyRhythm' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
polyRhythm' strokeFn _ oscs = scaleY nOscs . mconcat . P.map strokeFn . snd $ unzip oscs
  where nOscs = fromIntegral $ length oscs

syncCurve :: V2 Double -> Trail V2 Double
syncCurve vect = trailFromSegments [bezier3 (0.5 * y' D.*^ unitY) (r2 (x', 0.5 * y')) vect']
  where vect'    = vect ^+^ 0.5 D.*^ unitY
        x' :& y' = coords vect'

rhythmSync' :: (Trail V2 Double -> Diagram B) -> Double -> [(Colour Double, Trail V2 Double)] -> Diagram B
rhythmSync' _ sepa oscs = concat curves # P.map strokeLocTrail # mconcat # translateY (-0.5) # opacity 0.2
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

lblOscs :: Double -> [Diagram B] -> Diagram B
lblOscs = vdist

tones :: Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
tones = polyOsc tone (oscs strokeTone) lblOscs

rhythms :: Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
rhythms = polyOsc rhythm (oscs strokeRhythm) lblOscs

lblPolyOsc :: Double -> [Diagram B] -> Diagram B
lblPolyOsc _ = vdist 1

chord :: Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
chord = polyOsc tone (chord' strokeTone) lblPolyOsc

polyRhythm :: Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
polyRhythm = polyOsc rhythm (polyRhythm' strokeRhythm) lblPolyOsc

rhythmSync :: Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
rhythmSync = polyOsc rhythm (rhythmSync' strokeRhythm) (\_ _ -> mempty)

rhythmsWithSync :: Int -> [(Colour Double, Int)] -> Vector Double -> Int -> Diagram B
rhythmsWithSync w0 pcs tn f0 = rhythms w0 pcs tn f0 `atop` rhythmSync w0 pcs tn f0

farey :: Vector Double
farey = fromList [1, 16/15, 9/8, 6/5, 5/4, 4/3, 17/12, 3/2, 8/5, 5/3, 16/9, 15/8]

eqt :: Vector Double
eqt = fromList [2**(i / 12) | i <- [0 .. 11]]
