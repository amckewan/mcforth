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

