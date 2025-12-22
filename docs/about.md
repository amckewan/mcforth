# Goals

The main (and perhaps only) goal is to have some fun.
I enjoy building and tinkering with the internals of a Forth system.
To me this means:

* Small and simple
* Not slow
* Potentially portable

I'm not sure if I met these but I *am* having fun!

# History

I haven't done much Forth in the last 20 years, but I was looking
through my archives and I ran across something I had written back
in 1994. It was inspired by cmForth and ThisForth, combining
cmForth's dual-wordlist interpreter with the forth-in-C engine
from ThisForth.

It has changed and grown quite a bit in the past few months.
The main difference is that I now have a more traditional
interpreter using standard wordlists and an immediate flag.
The history is in git, including a working cmforth branch.

# Credits

Many others came before me and inspired me. I've looked at lots of
Forth implementations and written several different types of systems.
There may be something original here but most if it has probably
been tried before.

A few systems are especially noteworthy:

## Fig-Forth
This is where it started for me. At Oakland University in about 1982,
Professor Richard Haskell invited me to do a project in Forth. He provided
a printed assembler listing of the 8086 Fig-Forth. About an inch thick.

I typed it in (probably using edlin) and got it to build
and run on an IBM PC clone.

Of course all the learning happened between when I typed in the last
line and it assembled and finally ran. One of the great features
of Fig-Forth is that every byte was shown so everything could be understood.

## F83
F83 is absolutely amazing; a complete and free Forth system with editor,
assembler and a metacompiler. I still refer to it today.

## cmForth
Chuck showed a completely different approach to the interpreter, with 
dual vocabularies and an optimizing compiler. A real work of art.

## ThisForth
Wil Baden presented this at FORML. It uses the m4 macro processor
as a metacompiler to produce C source that is compiled to produce the
executable. Another great work of art, from a great man and friend.

## gforth
I used gforth as a stable place to start. It's a great standard system
that is well documented.
