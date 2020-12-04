# Virtual Machine

The Forth virtual machine is a byte-code interpreter implemented in C.

It can be built as either a 32-bit or 64-bit executable.
The 64-bit version does not properly implement the double-number
multiply and divide functions (e.g. um/mod) but is otherwise functional.
The default is 32 bits.

# Memory

A cell is 4 or 8 bytes and matches the 32/64 bit compiler so that a cell can hold an address.

All forth addresses are offsets from the base of memory 'm'.
These are converted to/from abolute addresses in the engine as required.

Since the dictionary contains no absolute addresses,
it is relocatable which allows the executable to contain
a copy of the dictionary.

The return stack grows down from the top of memory.

The data stack grows down in a separate area of memory.
The top of the stack is cached in the variable 'top'.
The next stack entry is S[0], then S[1], etc.
The bottom of the stack (or maybe top since it groes down) is S0.

The first X cells of memory are used for variables at fixed offsets
that are shared between Forth and C.
Some of these are also known by the target compiler so must
only be changed with caution.

| offset | variable  | notes
| -------| ----------| -----
|  0     |           | cold-start xt
|  1     |           | warm start (abort)
|  2     | H         | dictionary pointer
|  3     | BASE      |
|  4     | STATE     |
|  5     | 'IN       | points to current input source
|  6-7   | FORTH-WORDLIST | Forth wordlist
|  8     | CURRENT   | wordlist for new definitions
|  9-11  | CONTEXT   | search order wordlists (zero terminated)


# Forth Registers

Each of the Forth registers is local variable in the run() function.
They are only visible to the primatives defined in the kernel.

Register | Description
---- | ----------
I    | Instruction pointer
S    | Stack pointer
R    | Return-stack pointer
L    | Local frame pointer
top  | Top of stack
self | Object pointer
w    | Scratch register

# Opcodes

The opcodes are organized into 16 pages of 16 opcodes each.

The first 8 pages have specific assignments known to the compiler and optimizers.
Many of these opcodes have arguments following such as literals or
branch offsets. The remaining pages are for other CODE words in no particular order.

Opcodes 00-5F have operands and will not be automatically inlined. They are used
by COMPILER words that know what to do in each case.
There are several types of operands:

* Literals (cell bytes)
* Addresses (cell bytes, offset from m)
* Branch offsets (1 byte)
* Strings (1-byte count + chars)

Opcodes 60-FF have no operands and are always inlined.

Page | Description | Example
---- | ----------  | -------
0 | special functions | call, exit, abort"
1 | runtime | docon, dovar
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
1 | call | 16-bit offset, in cells, from m
2 | callx | cell offset from m
3 | branch | offset
4 | do | offset
5 | ?do | offset
6 | loop | offset
7 | +loop | offset
8 |
9 | nop
A | s" | counted string
B | ." | counted string
C | abort" | counted string
D | 
E | 
F | 

## Page 1: Runtime

opcode | function | operands
------ | -------- | -----
10 | docon| op, align, value
11 | dovar | op, align, value
12 | dodoes | op, I for DOES>, data
13 | dovalue | op, align, value
14 | dodefer | op, align, xt
15 | 
16 | 
17 |
18 | local{ | #locals, #params
19 | }local |
1A | local@ | local#
1B | local! | local#
1C |
1D |
1E |
1F |

## Page 2: Literals

Binary and memory operations followed by a literal (e.g. `5 +`)

20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 2A  | 2B  | 2C  | 2D  | 2E  | 2F
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
LIT | +   | -   | *   | /   | AND | OR  | XOR | @   | !   | +!


## Page 3: Literal Conditionals

Conditional operands followed by a literal (e.g. `5 <`)

30  | 31  | 32  | 33  | 34  | 35  | 36  | 37  |     | 38  | 39  | 3A  | 3B  | 3C  | 3D  | 3E  | 3F
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
0=  | 0<  | 0>  | =   | <   | >   | U<  | U>  |     | 0<> | 0>= | 0<= | <>  | >=  | <=  | U>= | U<=

## Page 4: Literal Compare and Branch

Literal conditional branch (e.g. `5 < if`)

## Page 5: Branching

Conditional branch (e.g. `< if`)


## Page 6: Binary and Memory Operations

60  | 61  | 62  | 63  | 64  | 65  | 66  | 67  | 68  | 69  | 6A  | 6B  | 6C  | 6D  | 6E  | 6F
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
    | +   | -   | *   | /   | AND | OR  | XOR | @   | !

## Page 7: Conditionals

70-72 are unary conditionals  
73-77 are binary conditionals  
78-7f are compliments of the first 8  

70  | 71  | 72  | 73  | 74  | 75  | 76  | 77  |     | 78  | 79  | 7A  | 7B  | 7C  | 7D  | 7E  | 7F
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
0=  | 0<  | 0>  | =   | <   | >   | U<  | U>  |     | 0<> | 0>= | 0<= | <>  | >=  | <=  | U>= | U<=
