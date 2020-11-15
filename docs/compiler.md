# FORTH and COMPILER

## Optimizing Compiler

## Meta Compiler

cross.f kernel.f --[gforth]--> bootstrap.inc

bootstrap.inc --[cc]--> bootstrap


meta.f kernel.f --> fo rth --> kernel.inc


kernel.inc --[cc]--> fo

extend --[fo]--> forth.inc

forth.inc --> cc -> forth

./fo rth meta.f kernel.f -e ciao

## Search Order

There are two vocabularies, FORTH and COMPILER.

## Extended Search Order

For Objects (and perhaps others) it would be nice to have an "extra"
vocabulary that is searched first. This is a simple extension rather
than going to the super-general multi-wordlist solution.

Interpreting: CONTEXT, FORTH
Compiling: COMPILER, CONTEXT, FORTH

Need to add CURRENT replacing what is now CONTEXT

We could have the extra wordlist chain to FORTH like was done
in Fig Forth. This means the interpreter does not have to change much.

For the general solution, CONTEXT is now an array of X entries (standars says at least 8).
FIND searches them in order. We mark the end of the current order with a NULL.

COMPILER still has it's special place which is searched first when compiling.

## Local Variables

If we do locals we also need an additional vocabulary in the search order.
This one is transient so it could be added to either FORTH or COMPILER and then removed
at the end of the definition. If we add to COMPILER then locals get the special
"first always" treatment.

We we just need a temporary area of the dictionary to add the names, then cut them off at
at the end of the definition.

If in COMPILER, they can compile the code to fetch the value.

For TO it is more complicated because it has to look for them before values.
So the compiler TO must first search COMPILER (maybe check the cfa to see if it
is really a local), then FORTH (or CONTEXT).
