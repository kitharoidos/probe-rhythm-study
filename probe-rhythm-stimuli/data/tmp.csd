<CsoundSynthesizer>

<CsOptions>

--nodisplays

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 2
0dbfs = 1.0
giPort init 1
opcode FreePort, i, 0
xout giPort
giPort = giPort + 1
endop




instr 24

endin

instr 23
 event_i "i", 22, 21.341239735038045, 1.0e-2
endin

instr 22
 turnoff2 21, 0.0, 0.0
 turnoff2 20, 0.0, 0.0
 turnoff2 19, 0.0, 0.0
 turnoff2 18, 0.0, 0.0
 exitnow 
endin

instr 21
arl0 init 0.0
arl1 init 0.0
ar0, ar1 subinstr 20
ir7 = 1.0
ar2 upsamp k(ir7)
ir8 = 0.0
ir9 = 89.0
ir10 = 100.0
ar3 compress ar0, ar2, ir8, ir9, ir9, ir10, ir8, ir8, 0.0
ar0 = (ar3 * 0.8)
arl0 = ar0
ar0 compress ar1, ar2, ir8, ir9, ir9, ir10, ir8, ir8, 0.0
ar1 = (ar0 * 0.8)
arl1 = ar1
ar0 = arl0
ar1 = arl1
 outs ar0, ar1
endin

instr 20
ir1 FreePort 
krl0 init 10.0
ir5 FreePort 
 event_i "i", 19, 0.0, 21.341239735038045, ir5, ir1
 event_i "i", 18, 0.0, 0.5939960407750999, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 0.5939960407750999, 0.7880622748023282, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 1.382058315577428, 0.73429555024967, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 2.116353865827098, 0.27157880065644013, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 2.3879326664835383, 0.27336509237265305, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 2.6612977588561915, 0.9263192889572933, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 3.5876170478134846, 0.8966634018843915, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 4.484280449697876, 0.26707657976426097, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 4.751357029462137, 0.35509751258153266, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 5.10645454204367, 0.5037055343409976, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 5.610160076384667, 0.40551606928112577, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 6.015676145665793, 0.6768109197782667, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 6.69248706544406, 0.4475923566396311, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 7.140079422083691, 0.2695736548579047, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 7.409653076941596, 0.6415460243403779, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 8.051199101281973, 0.33684776624865764, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 8.388046867530631, 0.385804110226915, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 8.773850977757547, 0.35875294163209615, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 9.132603919389643, 0.6608750204295972, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 9.79347893981924, 0.3420302807343582, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 10.135509220553597, 0.4204448289382926, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 10.55595404949189, 0.40516755063881044, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 10.9611216001307, 0.6887133350361833, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 11.649834935166883, 0.6941918110799705, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 12.344026746246854, 0.8412437298754483, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 13.185270476122302, 0.5558895203910992, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 13.741159996513401, 0.4813373078018984, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 14.222497304315299, 0.30954715274904454, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 14.532044457064343, 0.35881511196353344, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 14.890859569027876, 0.2995840887327452, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 15.190443657760621, 0.5487844606141927, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 15.739228118374815, 0.29311187675802286, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 16.032339995132837, 0.6008943541434395, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 16.633234349276275, 0.5640756847690253, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 17.1973100340453, 0.7797255288806363, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 17.977035562925938, 0.5513779257717824, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 18.52841348869772, 0.479598090471112, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 19.00801157916883, 0.49904739030338774, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 19.50705896947222, 0.912180475235682, 0.9999999999999999, 1.0, ir5
 event_i "i", 18, 20.4192394447079, 0.9220002903301424, 0.9999999999999999, 1.0, ir5
krl0 = 2.0
kr0 = krl0
S93 sprintf "alive_%d", ir5
 chnset kr0, S93
krl0 = 2.0
kr0 = krl0
S100 sprintf "alive_%d", ir1
 chnset kr0, S100
S103 sprintf "p1_%d", ir1
ar0 chnget S103
S106 sprintf "p2_%d", ir1
ar1 chnget S106
 chnclear S103
 chnclear S106
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
endin

instr 19
S1 sprintf "p1_%d", p4
ar0 chnget S1
S4 sprintf "p2_%d", p4
ar1 chnget S4
 chnclear S1
 chnclear S4
arl0 init 0.0
arl0 = ar0
ar0 = arl0
S17 sprintf "p1_%d", p5
 chnmix ar0, S17
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S26 sprintf "p2_%d", p5
 chnmix ar0, S26
endin

instr 18
ir1 = 8.5e-2
ir2 = birnd(ir1)
kr0 = birnd(ir1)
ir6 = 8.5e-3
kr1 = birnd(ir6)
ir9 = 1.0
ir10 = rnd(ir9)
ir12 = 0.0
ar0 noise ir9, ir12
 xtratim 0.1
ir17 = 9.0e-2
kr2 = birnd(ir17)
ir20 = birnd(ir1)
kr3 = birnd(ir1)
kr4 = birnd(ir6)
ir26 = rnd(ir9)
ar1 noise ir9, ir12
 xtratim 0.1
kr5 = birnd(ir17)
arl0 init 0.0
ir36 = (ir2 * 0.8)
ir37 = (0.8 + ir36)
ir38 = (2.7e-2 * ir37)
ar2 expsega 1.0, ir38, 1.0e-3, 1.0, 1.0e-3
ar3 = (ar2 - 1.0e-3)
ar2 = (0.5 * ar3)
kr6 = (kr1 * 1700.0)
kr1 = (1700.0 + kr6)
kr6 = (kr0 * 0.7)
kr0 = octave(kr6)
kr6 = (kr1 * kr0)
ar3 oscil3 ir9, kr6, 2, ir10
kr0 = (kr6 * 8.0)
ar4 butbp ar3, kr6, kr0
ar3 = (ar2 * ar4)
ir51 = (ir38 - 2.0e-3)
ir52 = (ir51 - 5.0e-3)
ar2 expsega 1.0, 2.0e-3, 0.8, 5.0e-3, 0.5, ir52, 1.0e-4, 1.0, 1.0e-4
ar4 = (ar2 - 1.0e-3)
kr0 expsegr 4000.0, ir38, 20.0, 1.0, 20.0, ir38, 20.0
ar2 butlp ar0, kr0
ar0 = (ar4 * ar2)
ar2 = (ar3 + ar0)
ar0 = (0.8 * ar2)
kr0 = birnd(ir17)
ar2 upsamp kr0
ar3 = (1.0 + ar2)
ar2 = (ar0 * ar3)
ar0 = (p4 * ar2)
arl0 = ar0
ar0 = arl0
S68 sprintf "p1_%d", p6
 chnmix ar0, S68
arl1 init 0.0
ir73 = (ir20 * 0.8)
ir74 = (0.8 + ir73)
ir75 = (2.7e-2 * ir74)
ar0 expsega 1.0, ir75, 1.0e-3, 1.0, 1.0e-3
ar2 = (ar0 - 1.0e-3)
ar0 = (0.5 * ar2)
kr0 = (kr4 * 1700.0)
kr1 = (1700.0 + kr0)
kr0 = (kr3 * 0.7)
kr3 = octave(kr0)
kr0 = (kr1 * kr3)
ar2 oscil3 ir9, kr0, 2, ir26
kr1 = (kr0 * 8.0)
ar3 butbp ar2, kr0, kr1
ar2 = (ar0 * ar3)
ir88 = (ir75 - 2.0e-3)
ir89 = (ir88 - 5.0e-3)
ar0 expsega 1.0, 2.0e-3, 0.8, 5.0e-3, 0.5, ir89, 1.0e-4, 1.0, 1.0e-4
ar3 = (ar0 - 1.0e-3)
kr0 expsegr 4000.0, ir75, 20.0, 1.0, 20.0, ir75, 20.0
ar0 butlp ar1, kr0
ar1 = (ar3 * ar0)
ar0 = (ar2 + ar1)
ar1 = (0.8 * ar0)
kr0 = birnd(ir17)
ar0 upsamp kr0
ar2 = (1.0 + ar0)
ar0 = (ar1 * ar2)
ar1 = (p4 * ar0)
arl1 = ar1
ar0 = arl1
S105 sprintf "p2_%d", p6
 chnmix ar0, S105
S108 sprintf "alive_%d", p6
kr0 chnget S108
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S108
endin

</CsInstruments>

<CsScore>

f2 0 1024 10  0.971 0.269 4.1e-2 5.4e-2 1.1e-2 1.3e-2 8.0e-2 6.5e-3 5.0e-3 4.0e-3 3.0e-3 3.0e-3 2.0e-3 2.0e-3 2.0e-3 2.0e-3 2.0e-3 1.0e-3 1.0e-3 1.0e-3 1.0e-3 1.0e-3 2.0e-3 1.0e-3 1.0e-3

f0 604800.0

i 24 0.0 -1.0 
i 23 0.0 -1.0 
i 21 0.0 -1.0 

</CsScore>

</CsoundSynthesizer>