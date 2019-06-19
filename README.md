# esAsm

esAsm is a toy assembly-ish language meant to be fun to use. Everything is currently subject to change.


## Instruction list

|Opcode | Summary              | Syntax
|-------|----------------------|---
|shn    | Show as number       | shn val
|sha    | Show as ascii        | sha val
|mov    | Move                 | mov dest val
|add    | Add                  | add dest val val
|jmp    | Jump                 | jmp val
|jlt    | Jump if less than    | jlt val val val
|jgt    | Jump if greater than | jgt val val val
|jet    | Jump if equal to     | jet val val val
|inp    | Input                | inp dest
|hlt    | Halt                 | hlt
