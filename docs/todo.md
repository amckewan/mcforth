# To-do list

* path in forth
* make source-name a counted string
* move stuff from kernel to extend
* remove non-essential code words (keep opcodes trim)
* fm/mod from f83

## Optimizing compiler

* tail call optimization
* inline lits with prims (partially tried)
* specials (e.g. dup if )
* lit pick
* lit lit within
* make dup>r and r>drop compiler only
* maybe move r>drop below 60 so it doesn't inline?

## Objects

* Write primatives
* Add indexed instance variables

## Do later (or never)

* find alternative to readline
* add require
* improve documentation
* view source
* get .s to fully observe base (dot())
