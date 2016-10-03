<CsoundSynthesizer>
<CsOptions>
; Enable both realtime input and output
-odac    ;;; output
; -iadc    ;;; input
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 100
nchnls = 2

#define PORT 		# 9001 #

gihandle OSCinit $PORT

instr 1
    karg init 0
    prints "listening on port %i\n", $PORT
nxtmsg:
    kk  OSClisten gihandle, "/home", "i", karg
if (kk == 0) goto ex
    printks "rcv: %i\n", 1, karg
    kgoto nxtmsg
ex:
endin


</CsInstruments>
<CsScore>
i1 0 3600
e
</CsScore>
</CsoundSynthesizer>
