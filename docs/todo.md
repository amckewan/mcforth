# To-do list

* path in forth
* move stuff from kernel to extend
* remove non-essential code words (keep opcodes trim)
* make branch offsets 16 bits if it keeps being a problem

## Maybe do

* add standard locals

## Optimizing compiler

* inline lits with prims (partially tried)
* specials (e.g. dup if )
* lit pick
* lit lit within

## Objects

* Write primatives
* Add indexed instance variables

## Do later (or never)

* add require
* view source
* get .s to fully observe base (dot())

## Grand ideas

### Multithreading

### Library mechamism

This is beyond the simple dlopen/dlsym interface. The idea is that
we write a library in C that has a Forth interface. The Forth interpreter
loads it and adds the words to the dictionary. This can be used to
add code words not in the kernel, and not worry about the 256 opcode limit.

But why is this really different than using dlopen? We could write
a library whose functions take M and SP and manipulate those as needed.

: setmem  0 >abs 1 call setmem drop ;
: dosomething  'S >abs 1 call dosomething ( drop ) ;

    int dosomething(cell **S) {
        cell *sp = *S;
        cell sum = sp[1] + sp[0]
        *++sp = sum;
        *S = sp;
    }

Or each libarary could have a single function "get_exports" that
has a list of name/function pairs, along with # in and out args
(this is where it gets complicated) and we build the dictionary from that.

    : "create ( name len -- ) forth-wordlist (header) $11 , ;
    : zcreate  dup strlen "create ;
    : strlen  dup begin count 0= until 1- swap - ;

    : external ( name sym #in #out -- )
        ( create word from name ) , , ,
        does> .... dlcall ... ;

    Export exports[] = {
        {"init-me", initme, 0, 0},
        {"doit", doit, 3, 1},
        {0}
    }
    exports get_exports() { return exports; }


