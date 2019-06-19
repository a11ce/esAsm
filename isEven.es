inp r0

l:
sub r0 r0 #2
jet y r0 #0
jlt n r0 #0
jmp l

y:
sha #89
jmp #0

n:
sha #78