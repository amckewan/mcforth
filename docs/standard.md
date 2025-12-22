# Standard Forth

What would a (more) standard system look like, and would it
lose some of what makes this interesting?

What is the goal after all?

If it's just to make a toy, for myself, then who cares?
If I actually want to share it, then a standard system is
much more valuable. If people can't do IMMEDIATE then it won't go far.

Besides, the reason for the standard is to share, so this makes one
more way to do it.

Ok, so what would it look like?

## Engine

Not much changes here except for perhaps find. And all that really needs
is to put back the immmediate flag.

## Dictionary Search

We could just have one wordlist, at least start with that. So we could
allocate CONTEXT to handle a bunch but put that stuff in later.

Except for optimizing compiler this would be pretty straightforward.

## Optimizing Compiler

COMPILE, already handles a few things:

1. Inline primatives
2. Inline constant, variable, etc.
3. Default to a call

What else do we optimize?

Conditionals: nothing special needed, the compiler words are perfectly capable of
doing this.

Binary/Unary: easy enough to add to COMPILE,

It makes COMPILE, a bit messy but it works. Maybe later we can find a way
simplify it. Perhaps a chain of optimizers that we can to later.

## Header Format

Add back the immediate bit.
Consider removing smudge and just doing reveal like in F83.
We only have to worry about ; in the metacompiler, easy enough to work around.

Hash table idea from gforth might be effective to speed up compiliation, but
something we can do later. For now lets keep it simple, single linked list.
Better to just not have so many words :)



