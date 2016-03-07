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



instr 46

endin

instr 45
 event_i "i", 44, 30.0, 1.0e-2
endin

instr 44
 turnoff2 43, 0.0, 0.0
 turnoff2 42, 0.0, 0.0
 turnoff2 41, 0.0, 0.0
 turnoff2 40, 0.0, 0.0
 turnoff2 39, 0.0, 0.0
 turnoff2 38, 0.0, 0.0
 turnoff2 37, 0.0, 0.0
 turnoff2 36, 0.0, 0.0
 turnoff2 35, 0.0, 0.0
 turnoff2 34, 0.0, 0.0
 turnoff2 33, 0.0, 0.0
 turnoff2 32, 0.0, 0.0
 turnoff2 31, 0.0, 0.0
 turnoff2 30, 0.0, 0.0
 turnoff2 29, 0.0, 0.0
 turnoff2 28, 0.0, 0.0
 turnoff2 27, 0.0, 0.0
 turnoff2 26, 0.0, 0.0
 turnoff2 25, 0.0, 0.0
 turnoff2 24, 0.0, 0.0
 turnoff2 23, 0.0, 0.0
 turnoff2 22, 0.0, 0.0
 turnoff2 21, 0.0, 0.0
 turnoff2 20, 0.0, 0.0
 turnoff2 19, 0.0, 0.0
 turnoff2 18, 0.0, 0.0
 exitnow 
endin

instr 43
arl0 init 0.0
arl1 init 0.0
ar0, ar1 subinstr 25
ar2, ar3 subinstr 30
ar4 = (ar0 + ar2)
ar0, ar2 subinstr 35
ar5 = (ar4 + ar0)
ar0 = (ar5 * 0.3333333333333333)
ar4 clip ar0, 0.0, 0dbfs
ar0 = (ar4 * 0.8)
arl0 = ar0
ar0, ar4 subinstr 42
ar5 clip ar4, 0.0, 0dbfs
ar4 = (ar5 * 0.8)
arl1 = ar4
ar4 = arl0
ar5 = arl1
 outs ar4, ar5
endin

instr 42
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 41
    ir13 = 10.0
    ir14 = 604800.0
     event "i", ir12, ir13, ir14, ir3
endif
S19 sprintf "p1_%d", ir3
ar0 chnget S19
S22 sprintf "p2_%d", ir3
ar1 chnget S22
 chnclear S19
 chnclear S22
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S45 sprintf "alive_%d", ir3
 chnset kr0, S45
endin

instr 41
arl0 init 0.0
ar0, ar1 subinstr 40
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 40
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 39
    ir13 = 20.0
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 39
arl0 init 0.0
ar0, ar1 subinstr 38
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 38
krl0 init 10.0
ir3 FreePort 
ir5 = 3.75
kr0 metro ir5
if (kr0 == 1.0) then
    krl0 = 2.0
    ir11 = 37
    ir12 = 0.0
    ir13 = 0.26666666666666666
     event "i", ir11, ir12, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 37
arl0 init 0.0
ar0, ar1 subinstr 36
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 36
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 20
    ir13 = 0.26666666666666666
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 35
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 34
    ir13 = 30.0
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 34
arl0 init 0.0
ar0, ar1 subinstr 33
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 33
krl0 init 10.0
ir3 FreePort 
ir5 = 3.0
kr0 metro ir5
if (kr0 == 1.0) then
    krl0 = 2.0
    ir11 = 32
    ir12 = 0.0
    ir13 = 0.3333333333333333
     event "i", ir11, ir12, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 32
arl0 init 0.0
ar0, ar1 subinstr 31
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 31
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 20
    ir13 = 0.3333333333333333
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 30
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 29
    ir13 = 30.0
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 29
arl0 init 0.0
ar0, ar1 subinstr 28
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 28
krl0 init 10.0
ir3 FreePort 
ir5 = 2.4
kr0 metro ir5
if (kr0 == 1.0) then
    krl0 = 2.0
    ir11 = 27
    ir12 = 0.0
    ir13 = 0.4166666666666667
     event "i", ir11, ir12, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 27
arl0 init 0.0
ar0, ar1 subinstr 26
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 26
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 20
    ir13 = 0.4166666666666667
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 25
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 24
    ir13 = 30.0
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 24
arl0 init 0.0
ar0, ar1 subinstr 23
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 23
krl0 init 10.0
ir3 FreePort 
ir5 = 2.0
kr0 metro ir5
if (kr0 == 1.0) then
    krl0 = 2.0
    ir11 = 22
    ir12 = 0.0
    ir13 = 0.5
     event "i", ir11, ir12, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 22
arl0 init 0.0
ar0, ar1 subinstr 21
arl0 = ar0
ar0 = arl0
S9 sprintf "p1_%d", p4
 chnmix ar0, S9
arl1 init 0.0
arl1 = ar1
ar0 = arl1
S19 sprintf "p2_%d", p4
 chnmix ar0, S19
S22 sprintf "alive_%d", p4
kr0 chnget S22
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S22
endin

instr 21
krl0 init 10.0
ir3 FreePort 
ir5 = 0.0
ar0 mpulse k(ksmps), ir5, 0.0
kr0 downsamp ar0, ksmps
if (kr0 == 1.0) then
    krl0 = 2.0
    ir12 = 20
    ir13 = 0.5
     event "i", ir12, ir5, ir13, ir3
endif
S18 sprintf "p1_%d", ir3
ar0 chnget S18
S21 sprintf "p2_%d", ir3
ar1 chnget S21
 chnclear S18
 chnclear S21
arl1 init 0.0
arl2 init 0.0
arl1 = ar0
arl2 = ar1
ar0 = arl1
ar1 = arl2
 outs ar0, ar1
kr0 = krl0
S44 sprintf "alive_%d", ir3
 chnset kr0, S44
endin

instr 20
arl0 init 0.0
ar0 subinstr 19
ir5 = 4000.0
ir6 = 0.1
ar1 moogladder ar0, ir5, ir6
ar0 = (3.0 * ar1)
arl0 = ar0
ar1 = arl0
S13 sprintf "p1_%d", p4
 chnmix ar1, S13
arl1 init 0.0
arl1 = ar0
ar0 = arl1
S22 sprintf "p2_%d", p4
 chnmix ar0, S22
S25 sprintf "alive_%d", p4
kr0 chnget S25
if (kr0 < -10.0) then
     turnoff 
endif
kr1 = (kr0 - 1.0)
 chnset kr1, S25
endin

instr 19
krl0 init 10.0
ir3 FreePort 
ir5 = 1.6666666666666666e-2
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

i 46 0.0 -1.0 
i 45 0.0 -1.0 
i 43 0.0 -1.0 

</CsScore>

</CsoundSynthesizer>