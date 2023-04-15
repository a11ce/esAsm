#lang esAsm
; take numeric input and store it in register 0
; input removed for testing
;inp r0
mov r0 #27
; take numeric input and store it in register 1
;inp r1
mov r1 #42

; jump to less if r0 < r1
jlt less r0 r1
; jump to greater if r0 > r1
jgt greater r0 r1
; jump to equal if r0 == r1
jet equal r0 r1

less:
; show L using ascii decimal
sha #76
; stop without continuing (to other branches)
hlt

greater:
; show G using ascii decimal
sha #71
; stop without continuing (to other branch)
hlt

equal:
; show G using ascii decimal
sha #69
; stop (don't loop)
hlt
