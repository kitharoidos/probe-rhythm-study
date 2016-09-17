module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Lib

main :: IO ()
main = multiMain
  [ ("polyRhythms", polyRhythms)
  , ("tonesToRhythms", tonesToRhythms)
  ]

polyRhythms :: Diagram B
polyRhythms = hsep 2
  [vsep 2 $ text lbl : [centerXY $ rhythmsWithSync w0 pc tn f0 | pc <- pcs] | (lbl, tn) <- tns]
  where pbs = [[], [(red, 2)], [(red, 1)]]
        cxt = [(black, 0), (black, 4), (black, 7)]
        pcs = map (cxt ++) pbs
        w0  = 15
        f0  = 1
        tns = [("12TET", eqt), ("Farey", farey)]

tonesToRhythms :: Diagram B
tonesToRhythms = hsep 4
  [ vsp [text "original chord", tones w1 pcs tn f1, chord w1 pcs tn f1]
  , vsp [text "frequencies scaled down", tones w2 pcs tn f2, chord w2 pcs tn f2]
  , vsp [text "waves transformed to ticks", rhythms w2 pcs tn f2, polyRhythm w2 pcs tn f2]
  ]
  where w0  = 7
        pcs = [(black, 0), (black, 4), (black, 7), (red, 11)]
        tn  = eqt
        f1  = 5
        f2  = 1
        w1  = w0 * f1
        w2  = w0 * f2
        vsp = vsep 2 . map centerXY
