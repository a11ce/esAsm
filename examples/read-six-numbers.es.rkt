#lang esAsm

; input replaced for testing

;inp r0
mov r0 #1

;inp r1
mov r1 #2

add r0 r0 r1

;inp r1
mov r1 #3

add r0 r0 r1
;inp r1
mov r1 #4

add r0 r0 r1

;inp r1
mov r1 #5

add r0 r0 r1

;inp r1
mov r1 #6

add r0 r0 r1
shn r0
hlt
