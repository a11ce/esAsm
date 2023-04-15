#lang esAsm
; take numeric input and store it in register 0
; input removed for testing
;inp r0
mov r0 #42

; loop label
l:

; subtract 2 from r0 and store it back in r0
sub r0 r0 #2

; if r0 is 0, the input was even so jump to yes
jet y r0 #0

; if r0 < 0, the input was odd so jump to no
jlt n r0 #0

; otherwise, we don't know yet so loop again
jmp l


y:
; print 'Y'
sha #89
; found an even number! we're done
hlt

n:
; print 'N'
sha #78
; implicit loop (until we get an even number)
