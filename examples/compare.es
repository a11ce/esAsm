inp r0
inp r1

jlt less r0 r1
jgt greater r0 r1
jet equal r0 r1

less:
sha #76
hlt

greater:
sha #71
hlt

equal:
sha #69
hlt