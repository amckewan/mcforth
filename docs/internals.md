# Memory

A cell is 4 bytes. Becuase we need to reference addresses in a cell we must use a 32-bit compiler. TODO remove this restriction through some cleverness.

All forth addresses are offsets from the base of memory 'm'. These are converted to/from abolute addresses in the engine as required.

Since the dictionary contains no absolute addresses, it is relocatable which allows the executable to contain a copy of the dictionary.

The return stack grows down from the top of memory.

The data stack grows down in a separate area of memory. The top of the stack is cached in the variable 'top'. The next stack entry is S[0], then S[1], etc. The bottom of the stack (or maybe top since it groes down) is S0.

The first X byte of memory are used for variables at fixed offsets that are shared between Forth and C. Some of these are also known b the target compiler so must only be changed with caution.

| offset    | variable  | notes
| ----------| ----------| -----
| 00        |           | cold-start xt
| 04        |           | warm start (abort)
| 08        | H         | dictionary pointer
| 0C        | BASE
| 10        | STATE
| 14        | 'IN       | points to current input source
| 18        | CONTEXT
| 1C        |           | forth vocabulary
| 20        |           | compiler vocabulary
| 24        |           | NULL word to end each vocabulary
| 30        |           | next available slot

# Forth registers

Register | Description
--- | ---
I | Instruction pointer
S | Stack pointer
R | Return-stack pointer
top | Top of stack
w | Current opcode, scratch register

# Opcodes

The opcodes are organized into 16 pages of 16 opcodes each.

The first 8 pages have specific assignments known to the compiler and optimizers.
Many of these opcodes have arguments following such as literals or
branch offsets. The remaining pages are for other CODE words in no particular order.

Opcodes 00-5F have operands and will not be automatically inlined. They are used
by COMPILER words that know what to do in each case.
There are several types of operands:

* Literals (cell bytes)
* Addresses (cell bytes)
* Branch offsets (1 byte)
* Strings (count + 1 bytes)

Opcodes 60-FF have no operands and are always inlined.

Page | Description | Example
---- | ----------  | -------
0 | special functions | call, exit, loop, dovar
1 | runtime | dovar, docreate
2 | lit op | 5 +
3 | lit cond | 5 <
4 | lit cond branch | 5 < if
5 | cond branch | < if
6 | op | +
7 | cond | <


## Page 0: Special Functions

opcode | function | operands
------ | -------- | -----
0 | exit
1 | call | 16-bit address
2 | call32 | 32-bit address
3 | branch | offset
4 | do | offset
5 | ?do | offset
6 | loop | offset
7 | +loop | offset
8 | lit | value
9 | nop
A | s" | counted string
B | ." | counted string
C | abort" | counted string
D | ---
E | ---
F | ---

## Page 1: Runtime

opcode | function | operands
------ | -------- | -----
10 | docon| op, value
11 | dovar | op, align, value
12 | docreate | op, align, I for DOES>, data
XX | dovalue | op, align, value
XX | dodefer | op, align, xt

## Page 2: Literals

## Page 3: Conditionals

## Page 4: Literal Compare and Branch

## Page 5: Branching

## Page 6: Binary and Memory Operations

## Page 7: Conditionals
