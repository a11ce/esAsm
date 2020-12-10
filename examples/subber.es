; take numeric input and store it in register 0
inp r0

; take numeric input and store it in register 1
inp r1

; subtract r1 from r0 and store in r3
sub r3 r0 r1

; print r3
shn r3

; if r3 < 0, loop
jgt #0 r3 #0

; otherwise halt
hlt