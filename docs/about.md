# Goals

The main (and perhaps only) goal is to have some fun.
I enjoy building and tinkering with the internals of a Forth system.
To me this means:

* Simple and small
* Not slow
* Potentially portable

I'm not sure if I met these but I *am* having fun!

# History

I have many implementations of Forth in my library, mostly from the 1990s.
I wrote this one after the 1994 FORML,
inspired by ThisForth and cmForth.

I dug it up from the archives in 2020 and took it from there.
Along the way, it changed to a byte-code engine with
a more standard interpreter rather than the cmForth model.
The history is in git, including a still-working cmforth branch.

I experimented with using absolute instead of relative addresses.
This worked but I decided the complexity
of building and loading relocatable dictionaries wasn't worth it.
And I didn't see the substantial performance improvement I wanted.

# Credits

Many others came before me and inspired me. There may be
something original here but most if it has probably been tried before.

## Fig-Forth
This is where it started for me. At Oakland University in about 1981,
Prof. Richard Haskell invited me to do a project in Forth. He provided
a printed assembler listing of the 8086 Fig-Forth. About 2 inches thick.

I typed it in (probably using edlin) and got it to compile
and run on an IBM PC clone.

Of course all the learning happened between when I typed in the last
line and it assembled and finally ran. One of the great features
of Fig-Forth is that every byte was shown so everything could be understood.

I modified it to run on a microcontroller (perhaps a 6800 family)
for my final project which was a robot.

## F83
This one really opened thing up. Absolutely amazing.
A metacompiler, forth-in-forth, assembler, editor.
A complete and free Forth system. Lots of time learning and studying.

I used this on my first software job (1985-88) with a company that had
never used Forth before. We used a RCA 1806 processor (wonderful for Forth)
to build hand-held data colletion terminals. I used Forth not only for
the embedded code but also for an extension language allowing users to 
develop custom software for the device. It probably would have gone over
better if I'd moved away from blocks but so be it.

(Ok, this is supposed to be about McForth not McKewan so I will stop now.)

## cmForth
Chuck showed a completely different approach to the interpreter, with 
dual vocabularies and an optimizing compiler. A real work of art.

## ThisForth
Wil Baden presented this at FORML. It use the m4 macro processor
as a metacompiler to produce C source that was compiled to produce the
executable. This is also a great work of art. From a great man and friend.
This is basically what I copied.

## gforth
I used gforth as a stable place to start. It's a great, complete, free system
that is well documented. Nothing wrong with it, I just wanted to write my own.
