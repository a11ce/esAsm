inp r0

mov r2 #0

s:
add r2 r2 #1
mov r1 r0
jgt e r2 r0
jmp cs


cs:
sub r1 r1 r2
jet y r1 #0
jlt s r1 #0
jmp cs

y:
shn r2
jmp s

e:
hlt