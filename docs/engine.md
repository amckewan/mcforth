# Memory

All forth addresses are offsets from the base of memory 'm'.

The return stack grows down from the top of memory.

The data stack grows down in a separate area of memory. The top of the stack is cached in the variable 'top'. The next stack entry is S[0], then S[1], etc. The bottom of the stack (or maybe top since it groes down) is S0.

The first

# Machine registers

Register | Description
--- | ---
I | Instruction pointer
S | Stack pointer
R | Return-stack pointer
top | Top of stack
w | Current opcode, scrach register

# Opcodes


The opcodes are grouped into 16 pages of 16 opcode each.

The first 8 pages have specific assignments known to the compiler and optimizers.
Many of these opcodes have arguments following such as literals or
branch destinations.
Opcodes XX and greater have no arguments or particular numeric assignments.


Page | Description | Example
---- | ----------  | -------
0 | special functions | call, exit, loop, dovar
1 | lit op | 5 +
2 | lit cond | 5 <
3 | lit cond branch | 5 < if
4 | cond branch | < if
5 | op | +
6 | cond | <
7 | reserved | in case we want more stuff


## Page 0: Special Functions

opcode | function | operands
------ | -------- | -----
0 | exit
1 | call | address
2 | branch | address
3 | do | address
4 | ?do | address
5 | loop | address
6 | +loop | address
7 | lit | value
8 | variable
9 | create
A | S" | counted string
B | ." | counted string
C | abort" | counted string
D | unused
E | unused
F | unused


Page 1:

Page 2: literals


00 |01 | 02
--- | ---  | ---
EXIT | +  | lit +
X    | -  | lit -
X    | *  | lit *
X    | /  | lit /
X    | mod | lit mod
X    | and |
X    | or
X    | xor

