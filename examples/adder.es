; take numeric input and store it in register 0
inp r0

; take numeric input and store it in register 1
inp r1

; add the contents of r0 and the contents of r1
; and store it in r2
add r2 r0 r1

; print the contents of r2 as a number
shn r2

; end execution (don't loop)
hlt