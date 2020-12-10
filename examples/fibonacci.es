; initial values
mov r0 #1
mov r1 #1
shn #1
shn #1
loopStart:
; add to find next term
add r2 r0 r1

; show result
shn r2

; shift previous terms
mov r0 r1
mov r1 r2

; increment term counter
add r3 r3 #1
; stop at term 10 (including two initial)
jet end r3 #10
; loop if not stopping
jmp loopStart
end:
hlt