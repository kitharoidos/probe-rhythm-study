<CsoundSynthesizer>

<CsOptions>

--output=dac --nodisplays

</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 64
nchnls = 1
0dbfs = 1.0
giPort init 1
opcode FreePort, i, 0
xout giPort
giPort = giPort + 1
endop



instr 25

endin

instr 24
 event_i "i", 23, 10.0, 1.0e-2
endin

instr 23
 turnoff2 22, 0.0, 0.0
 turnoff2 21, 0.0, 0.0
 turnoff2 20, 0.0, 0.0
 turnoff2 19, 0.0, 0.0
 turnoff2 18, 0.0, 0.0
 exitnow 
endin

instr 22
arl0 init 0.0
ar0 subinstr 19
ir5 = 4000.0
ir6 = 0.1
ar1 moogladder ar0, ir5, ir6
ar0 = (3.0 * ar1)
ar1 subinstr 20
ar2 moogladder ar1, ir5, ir6
ar1 = (3.0 * ar2)
ar2 = (ar0 + ar1)
ar0 subinstr 21
ar1 moogladder ar0, ir5, ir6
ar0 = (3.0 * ar1)
ar1 = (ar2 + ar0)
kr0 linseg 0.0, 1.0, 1.0, 8.0, 1.0, 1.0, 0.0, 1.0, 0.0
ar0 upsamp kr0
ar2 = (ar1 * ar0)
ar0 clip ar2, 0.0, 0dbfs
ar1 = (ar0 * 0.8)
arl0 = ar1
ar0 = arl0
 out ar0
endin

instr 21
krl0 init 10.0
ir3 FreePort 
ir5 = 3.9954855383378174
kr0 metro ir5
if (kr0 == 1.0) then
    krl0 = 2.0
    ir11 = 18
    ir12 = 0.0
    ir13 = 0.5
    ir14 = 0.5
     event "i", ir11, ir12, ir13, ir14, ir3
endif
S19 sprintf "p1_%d", ir3
ar0 chnget S19
 chnclear S19
arl1 init 0.0
arl1 = ar0
ar0 = arl1
 out ar0
kr0 = krl0
S34 sprintf "alive_%d", ir3
 chnset kr0, S34
endin

instr 20
krl0 init 10.0
ir3 FreePort 
ir5 = 3.359789466386329
kr0 metro ir5
if (kr0 == 1.0) then
    krl0 = 2.0
    ir11 = 18
    ir12 = 0.0
    ir13 = 0.5
    ir14 = 0.5
     event "i", ir11, ir12, ir13, ir14, ir3
endif
S19 sprintf "p1_%d", ir3
ar0 chnget S19
 chnclear S19
arl1 init 0.0
arl1 = ar0
ar0 = arl1
 out ar0
kr0 = krl0
S34 sprintf "alive_%d", ir3
 chnset kr0, S34
endin

instr 19
krl0 init 10.0
ir3 FreePort 
ir5 = 2.6666666666666665
kr0 metro ir5
if (kr0 == 1.0) then
    krl0 = 2.0
    ir11 = 18
    ir12 = 0.0
    ir13 = 0.5
    ir14 = 0.5
     event "i", ir11, ir12, ir13, ir14, ir3
endif
S19 sprintf "p1_%d", ir3
ar0 chnget S19
 chnclear S19
arl1 init 0.0
arl1 = ar0
ar0 = arl1
 out ar0
kr0 = krl0
S34 sprintf "alive_%d", ir3
 chnset kr0, S34
endin

instr 18
ir1 = 5.0e-2
ir2 = birnd(ir1)
kr0 = birnd(ir1)
ir6 = 5.0e-3
kr1 = birnd(ir6)
ir9 = 1.0
ir10 = rnd(ir9)
ir12 = 0.0
ar0 noise ir9, ir12
 xtratim 0.1
ir17 = 9.0e-2
kr2 = birnd(ir17)
arl0 init 0.0
ir22 = (p4 + 1.0)
ir23 = (ir2 * ir22)
ir24 = (ir22 + ir23)
ir25 = (2.7e-2 * ir24)
ar1 expsega 1.0, ir25, 1.0e-3, 1.0, 1.0e-3
ar2 = (ar1 - 1.0e-3)
ar1 = (0.5 * ar2)
kr3 = (p4 + 0.5)
kr4 = (1200.0 * kr3)
kr3 = (kr1 * kr4)
kr1 = (kr4 + kr3)
kr3 = (kr0 * 0.7)
kr0 = octave(kr3)
kr3 = (kr1 * kr0)
ar2 oscil3 ir9, kr3, 1, ir10
kr0 = (kr3 * 8.0)
ar3 butbp ar2, kr3, kr0
ar2 = (ar1 * ar3)
ir40 = (ir25 - 2.0e-3)
ir41 = (ir40 - 5.0e-3)
ar1 expsega 1.0, 2.0e-3, 0.8, 5.0e-3, 0.5, ir41, 1.0e-4, 1.0, 1.0e-4
ar3 = (ar1 - 1.0e-3)
kr0 expsegr 4000.0, ir25, 20.0, 1.0, 20.0, ir25, 20.0
ar1 butlp ar0, kr0
ar0 = (ar3 * ar1)
ar1 = (ar2 + ar0)
ar0 = (0.8 * ar1)
kr0 = birnd(ir17)
ar1 upsamp kr0
ar2 = (1.0 + ar1)
ar1 = (ar0 * ar2)
ar0 = (p4 * ar1)
arl0 = ar0
ar0 = arl0
S57 sprintf "p1_%d", p5
 chnmix ar0, S57
S60 sprintf "alive_%d", p5
kr0 chnget S60
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S60
endin

</CsInstruments>

<CsScore>

f1 0 1024 10  0.971 0.269 4.1e-2 5.4e-2 1.1e-2 1.3e-2 8.0e-2 6.5e-3 5.0e-3 4.0e-3 3.0e-3 3.0e-3 2.0e-3 2.0e-3 2.0e-3 2.0e-3 2.0e-3 1.0e-3 1.0e-3 1.0e-3 1.0e-3 1.0e-3 2.0e-3 1.0e-3 1.0e-3

f0 604800.0

i 25 0.0 -1.0 
i 24 0.0 -1.0 
i 22 0.0 -1.0 

</CsScore>

</CsoundSynthesizer>