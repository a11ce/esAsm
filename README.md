# esAsm

> esAsm is a toy assembly-ish language meant to be fun to use.

## Installation

- Download with `git clone https://github.com/a11ce/esAsm.git`
- Install with `python3 setup.py install`
- Run (anywhere) with `esAsm`

## Usage

- Run `esAsm <file>` to execute the given file.
- In the esAsm directory, run `python3 cppTranspiler.py <file>` to output transpiled C++ code or `./compile.sh <file>` to automatically compile it.

## Examples

There are a few examples in the `examples/` directory.

- `adder.es` adds two inputted numbers and outputs the result.
- `subber.es` subtracts the second inputted number from the first and outputs the result, halting if the difference is negative and looping otherwise.
- `isEven.es` outputs `Y` if the inputted number is even and `N` otherwise. 
- `compare.es` outputs `G`, `L`, or `E` if the first inputted number is respectively greater, less, or equal to the second.
- `fibonacci.es` outputs the first 10 terms of the Fibonacci sequence. It is also fully commented.

## The esAsm language

- Each line in an esAsm file may contain whitespace, a comment, a label, or an instruction.
- Comment lines begin with `;` and are ignored by the interpreter.
- Labels end with `:`.
- Literals are prefixed with `#`, and register references are prefixed with `r`.
- The program execution is moved to the beginning if it reaches the end of the file. It will continue looping until the `hlt` instructions.

### Instructions

In this documentation, `val` means any literal or register, `reg` means any register, and `lab` means any label.

- `shn val` prints the numeric value of x, which may be a literal or register.
- `sha val` prints the ascii character with the decimal value x.
- `mov reg val` sets the value of `reg` to `val`.
- `add reg val1 val2` sets the value of `reg` to `val1` + `val2`.
- `jmp lab` moves the program execution to `lab`.
- `jlt lab val1 val2` moves the program execution to `lab` if `val1` < `val2`.
- `jgt lab val1 val2` moves the program execution to `lab` if `val1` > `val2`.
- `jet lab val1 val2` moves the program execution to `lab` if `val1` == `val2`.
- `inp reg` sets the value of `reg` to an integer from stdin. If stdin is user input, a prompt (`>`) will be shown.
- `hlt` ends the execution of the program. 

## Instruction reference

|Opcode | Summary              | Syntax
|-------|----------------------|---
|shn    | Show as number       | shn val
|sha    | Show as ascii        | sha val
|mov    | Move                 | mov reg val
|add    | Add                  | add reg val1 val2
|jmp    | Jump                 | jmp lab
|jlt    | Jump if less than    | jlt lab val1 val2
|jgt    | Jump if greater than | jgt lab val1 val2
|jet    | Jump if equal to     | jet lab val1 val2
|inp    | Input                | inp reg
|hlt    | Halt                 | hlt


---

All contributions are welcome by pull request or issue.

esAsm is licensed under GNU General Public License v3.0. See [LICENSE](../master/LICENSE) for full text.