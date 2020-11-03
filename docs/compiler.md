# FORTH and COMPILER

# Optimizing Compiler

# Ideas from cmForth
RECURSIVE instead of REVEAL
Build target at 2000 (8000 for us)
SMUDGE has value in that we can forget any word, not just the last
\ for [COMPILE] but we don't want that
\\ for -opt
BEGIN for MARK

steps:
1) fix current kernel
add dA
identify what is relocatable and what is not
consider changing things like call to relative addresses
