# esAsm

> esAsm is a toy assembly-like language 

## Installation

`raco pkg install esAsm`

## The esAsm language

- Each line in an esAsm file may contain whitespace, a comment, a label, or an instruction.
- Comment lines begin with `;` and are ignored. Lines starting with `#lang esAsm` are also ignored.
- Labels are indicated with the suffix `:`. The `:` is not part of the label name when referenced elsewhere.
- Literal integers are prefixed with `#`, and register references are prefixed with `r`. The registers are `r0`-`r11`.
- Program execution is moved to the beginning of a file when the end is reached. It will continue looping until the `hlt` instruction.

### Instructions

In this documentation, `val` means any literal or register, `reg` means a register, and `lab` means a label.

- `shn val` prints the numeric value of `val`.
- `sha val` prints the ascii character with the decimal value `val`.
- `mov reg val` sets the value of `reg` to `val`.
- `add reg val1 val2` sets the value of `reg` to `val1` + `val2`.
- `sub reg val1 val2` sets the value of `reg` to `val1` - `val2`.
- `jmp lab` moves the program execution to `lab`.
- `jlt lab val1 val2` moves the program execution to `lab` if `val1` < `val2`.
- `jgt lab val1 val2` moves the program execution to `lab` if `val1` > `val2`.
- `jet lab val1 val2` moves the program execution to `lab` if `val1` == `val2`.
- `inp reg` sets the value of `reg` to an integer from read from stdin.
- `hlt` ends the execution of the program. 
- `mficofsr reg` sets the value of `reg` to the value of the "Is Computer On Fire" status register. See [here](https://twitter.com/ppcinstructions/status/559753895757742083).

## Examples

There are a few examples in the `examples/` directory.

- `read-six-numbers.es.rkt` reads six numbers and prints their sum.
- `is-even.es.rkt` prints `Y` if the given number is even and `N` if it's odd. 
- `compare.es.rkt` prints `G`, `L`, or `E` if the first number is respectively greater, less, or equal to the second.
- `fibonacci.es.rkt` prints the first 10 terms of the Fibonacci sequence. 
- `factors.es.rkt` prints the factors of a given number. 
- `no-jump.es.rkt` demonstrates the branchless sublanguage.

For testing purposes, `inp` is replaced (commented) in the examples.

## Instruction reference

|Name       | Summary                                          | Syntax
|-----------|--------------------------------------------------|---
|shn        | Show as number                                   | shn val
|sha        | Show as ascii                                    | sha val
|mov        | Move                                             | mov reg val
|add        | Add                                              | add reg val1 val2
|sub        | Subtract                                         | sub reg val1 val2
|jmp        | Jump                                             | jmp lab
|jlt        | Jump if less than                                | jlt lab val1 val2
|jgt        | Jump if greater than                             | jgt lab val1 val2
|jet        | Jump if equal to                                 | jet lab val1 val2
|inp        | Input                                            | inp reg
|hlt        | Halt                                             | hlt
|mficofsr   | Move From Is Computer On Fire Status Register    | mficofsr reg

---

All contributions are welcome by pull request or issue, especially new instructions.

Behavior of esAsm is undefined if the computer is on fire.

esAsm is licensed under the MIT license. See [LICENSE](../master/LICENSE) for full text.
