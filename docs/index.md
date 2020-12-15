# Welcome to McForth

## Directories

The top-level directory contains the Makefile which works under Linux.
It will need modification to work on another platform.
All build products also go here.

Directory | contents
--- | ---
bench | simple benchmarks
class | experimental object model
docs | some documentation, which you have now stumbled upon
lib | optional forth source, some of it compiled by extend
src | source for the engine, kernel and core words
test | unit tests

## Building

There are two parts to the system, the "engine" written in C and 
a dictionary image.
Some of the engine is in C source files and some is generated from kernel.f.

Here is the basic build process:

* Metacompile kernel.f to produce prims.inc and kernel.inc
* Compile the C source files and the inc files to produce the executable `fo`
* Run `fo extend` to produce forth.inc
* Compile the C sources with forth.inc to product the executable `forth`
* Run the unit tests

You will need gforth or other Forth to bootstrap from source.

    make bootstrap

Now that you have built forth, you can just do

    make forth

or

    make tests

## Cell size

By default make builds a 32-bit executable with 4-byte cells. This requires
installing the 32-bit libc and either libreadline or libedit.

You can build a 64-bit executable like this:

    make CELL=8 bootstrap

Not all of the unit tests pass because I have not bothered to
implement the 64-bit versions of words like M* and UM/MOD.

## Command-line options

The command-line options are similar to a subset of the gforth options.

Some command-line options are used (and consumed) by the engine.
The rest are passed to forth and which can use argc and argv to access them.

Option | Args | Purpose
--- | --- | ---
-v |   | Verbose mode, -vv -vvv more verbose
-i | image | load image file
-m | size | set dictionary size, defaults is 32K cells
-e | string | evaluate string

Non-option arguments are treated as filenames which are are included.

Options are processed sequentially as found, so you can combine include
files and -e options and they will be interpreted in order. For example:

    forth exit-on-error -e "warning on" lib/locals.f myapp.f -e bye

