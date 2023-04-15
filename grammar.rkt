#lang brag
; https://github.com/a11ce/esAsm#the-esasm-language

program : line*
@line : [(lab-decl | inst)] /NEWLINE

@val : num | reg
num : NUM
reg : REG
lab : LABEL

lab-decl : @lab

inst : SHN val
     | SHA val
     | MOV @reg val
     | ADD @reg val val
     | SUB @reg val val
     | JMP @lab
     | JLT @lab val val
     | JGT @lab val val
     | JET @lab val val
     | INP @reg
     | HLT
     | MFICOFSR @reg
