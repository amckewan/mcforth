# To-do list

* self-compile
* check for redefined words
* move stuff from meta to rth
* add value
* add defer
* add sysvar in meta
* remove non-essential code words (keep opcodes trim)
* fm/mod from f83

### Ideas from cmForth

* RECURSIVE instead of REVEAL
* Build target at 2000 (8000 for us)
* SMUDGE has value in that we can forget any word, not just the last
* \ for [COMPILE] but we don't want that
* \\ for -opt
* BEGIN for MARK

In current meta:

* identify what is relocatable and what is not
* consider changing things like call to relative addresses

## Optimizing compiler

* ok lit+op
* ok lit+cond
* ok cond+branch
* ok lit+cond+branch
* ok inline const/var
* inline 1+ etc. (single lit op then exit)
* multi-op inline (e.g. 2dup )
* specials (e.g. dup if )
* lit+pick

## Do later (or never)

* improve documentation
* use 64-bit compiler
* allow 32 or 64-bit to work
* view source
* fm/mod (steal from Wil)
