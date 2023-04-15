#lang esAsm/no-jump

mov r0 #40
mov r1 #2
add r0 r0 r1
shn r0

; no-jump doesn't loop!