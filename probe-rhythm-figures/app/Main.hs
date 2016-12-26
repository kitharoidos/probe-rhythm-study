{-# LANGUAGE FlexibleContexts #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List as L

import Lib

main :: IO ()
main = multiMain
  [ ("polyRhythms", polyRhythms)
  , ("tonesToRhythms", tonesToRhythms)
  ]

polyRhythms :: Diagram B
polyRhythms = frame 0.25 . fontSizeO 24 . centerXY $ vsep 3 allOscs
  where pbs = [[], [(red, 2)], [(red, 1)]]
        cxt = [(black, 0), (black, 4), (black, 7)]
        pcs = map (cxt ++) pbs
        w0  = 15
        f0  = 1
        tns = [("12TET", eqt), ("Farey", farey)]
        sp  = 2
        eqtOscs  = [ beside' unitX (centerY (labelOscs sp pc) ||| strutX 2)
                                   (centerXY $ rhythmsWithSync sp w0 pc eqt f0)
                   | pc <- pcs]
        farOscs  = [centerXY $ rhythmsWithSync sp w0 pc farey f0 | pc <- pcs]
        eqtOscs' = beside' (-unitY) (fontSizeO 32 $ centerX (text "12TET") ||| strutY 3) (head eqtOscs) : tail eqtOscs
        farOscs' = beside' (-unitY) (fontSizeO 32 $ centerX (text "Farey") ||| strutY 3) (head farOscs) : tail farOscs
        allOscs  = zipWith (\e f -> hsep 3 [e, f]) eqtOscs' farOscs'

tonesToRhythms :: Diagram B
tonesToRhythms = frame 0.25 . fontSizeO 24 . centerXY $ hsep 3 oscs'
  where w0   = 7
        pcs  = [(black, 0), (black, 4), (black, 7), (red, 11)]
        tn   = eqt
        f1   = 4
        f2   = 1
        w1   = w0 * f1
        w2   = w0 * f2
        sp   = 2
        titles    = map (centerX . fontSizeO 32 . text)
                        ["Original chord", "Frequencies scaled down", "Waves transformed to ticks"]
        oscs      = [ beside' unitX (centerY (labelOscs sp pcs) ||| strutX 1.5) (centerXY $ tones sp w1 pcs tn f1)
                    , centerXY $ tones sp w2 pcs tn f2, centerXY $ rhythms sp w2 pcs tn f2
                    ]
        polyOscs  = [ beside' unitX (centerY (labelPolyOsc sp pcs) ||| strutX 1.5) (centerXY $ chord sp w1 pcs tn f1)
                    , centerXY $ chord sp w2 pcs tn f2, centerXY $ polyRhythm sp w2 pcs tn f2
                    ]
        oscScales = map centerX [oscScale f1 "0.01 s", oscScale f2 "1 s", oscScale f2 "1 s"]
        oscs'     = zipWith4 (\t o p s -> vsep 1.5 [t, o, p, s]) titles oscs polyOscs oscScales
