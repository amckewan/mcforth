# Compiler and Interpreter

This section describes the compiler and interpreter.

This is a traditional model, with an INTERPRET loop that looks up each word
and compiles or executes it depending on STATE and the IMMEDIATE bit.

There is no support for double or floating point literals.

## Search Order

Currently just FORTH, but can be extended to support the standard
search-order words (lib/order.f).

## Optimizing Compiler

While originally using the cmForth dual-wordlist model, McForth now uses
a traditional immediate-flag model. One consequence is that the optimizing
compiler is now contained in more complex COMPILE, rather than being
distributed among words in the COMPILER vocabulary.

COMPILE, performs the following optimizations:

* inline primatives
* inline defining words (constant, variable)
* combine binary operators (e.g. +) that follow literals
* combine branches with preceeding conditional operations
* compile short call if possible
* tail-call optimization

## Meta Compiler

The meta compiler is used to compile the Forth kernel. It compiles
the kernel into a separate area of memory. The implementation was adapted
from cmForth and is quit simple because the host and target have exactly
the same model (dictionary structure, primatives, etc.).

The metacompiler produces two files, both of which are included in
fo.c which is compiled to produce the forth executable.

* prims.inc has the C code for all the primatives
* kernel.inc is the dictionary image for the kernel

The output of this step is the executable `fo`. This can be run, like ThisForth,
as `fo rth` for the full forth system.

We can also (and typically do) build an extended binary by running `fo extend`.
This saves the dicationary image as forth.inc, which can be included in fo.c
and compiled to product the forth executable.

The cross compiler allows us to bootstrap from gforth.

    gforth + cross.f + kernel.f --> prims.inc kernel.inc + fo.c --> fo
    fo + extend --> forth.inc + fo.c --> forth

Now we use forth to compile itself. This one is faster since it
uses the optimizing compiler to compile the kernel.

    forth + meta.f + kernel.f --> prims.inc kernel.inc + fo.c --> fo
    fo + extend --> forth.inc + fo.c --> forth

## Local Variables

Local variables are stored on the return stack using a new register L which
points to the local stack frame.

During compilation, local variables are stored in the `locals` worlist which
is pushed to the top of the search order. See lib/locals.f for more details.
