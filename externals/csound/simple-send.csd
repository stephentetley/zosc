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


instr 1
    OSCsend 1, "127.0.0.1", $PORT, "/home", "i", p4
endin


</CsInstruments>
<CsScore>
i1  0  1 1
i1 10  1 2
i1 20  1 3
i1 30  1 4
i1 40  1 5
i1 50  1 6
e
</CsScore>
</CsoundSynthesizer>
