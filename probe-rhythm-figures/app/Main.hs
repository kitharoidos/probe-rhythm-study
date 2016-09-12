module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List as L
import Data.Array.Repa as Arr
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Vector as V

import Lib

import Control.Arrow (second)

main :: IO ()
main = multiMain
  [ ("polyRhythms", polyRhythms)
  , ("chordToRhythm", chordToRhythm)
  ]

-- cxtPcs :: [Int]
-- cxtPcs = [0, 4, 7]
--
-- prbPcs :: [[Int]]
-- prbPcs  = [[], [1], [2], [3], [5], [6], [8], [9], [10], [11]]
--
-- pcs :: Array V.V DIM1 [(Colour Double, Int)]
-- pcs = fromList (Z :. 10) $ L.map (sortOn (negate . snd) . (L.++) cxtPcs') prbPcs'
--   where cxtPcs' = L.map ((,) black) cxtPcs
--         prbPcs' = L.map (L.map ((,) red)) prbPcs
--
-- tunings :: Array V.V DIM1 (Array U DIM1 Double)
-- tunings = fromList (Z :. 2) [eqt, farey]
--
-- f0s :: Array U DIM1 Double
-- f0s = fromListUnboxed (Z :. 4) $ [eqt ! (Z :. pc) | pc <- [0, 4, 8]] L.++ [2]
--
-- allPolyRhythms :: Array V.V DIM3 (Diagram B)
-- allPolyRhythms = computeS $ traverse3 pcs tunings f0s transExtent newElem
--   where transExtent (Z :. nP) (Z :. nT) (Z :. nF) = Z :. nP :. nT :. nF
--         newElem getP getT getF (Z :. iP :. iT :. iF) =
--           polyRhythm 16 (getP $ Z :. iP) (getT $ Z :. iT) (getF $ Z :. iF)
--
-- allChords :: Array V.V DIM3 (Diagram B)
-- allChords = computeS $ Arr.map (scaleX (1 / fromIntegral n) . hcat . replicate n) allPolyRhythms
--   where n = 10

polyRhythms :: Diagram B
polyRhythms = polyRhythm 15 [(black, 7), (black, 4), (black, 0)] farey 1

chordToRhythm :: Diagram B
chordToRhythm = unitSquare
