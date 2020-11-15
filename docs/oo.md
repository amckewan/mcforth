# Objects

- message passing
- `object selector` syntax

The main decision to make is message-passing (like Smalltalk) vs. virtual methods
(like C++/Java). Virtual methods are more efficient since they just need to index into
the virtual method table. Message passing requires a runtime lookup but is more flexible.

What would method lookup look like?

    xt find_method(cell methods, cell selector) {
        cell link = methods + selector & 0x1C; // 8 threads CELLS(THREADS - 1)
        while ((link = AT(link)) {
            if (AT(link + CELL) == selector) {
                return link + 2 * CELL;
            }
        }
        return 0;
    }

The first line is pretty much what a vtable implementation would do.
The search loop depends on the size of the method table as well as the number
of methods in the class.

The real advantage of message passing is that selectors are not tied to
the class. Two unrelated classes can respond to the same message in completely
different ways. Interfaces are not required. I'll go with this.

## Selectors

Objects just leave their address on the stack.
This allows simple dictionary, allocated, locals, etc.
Selectors are active, they find and execute a method.
So selectors have to be words and can use their PFA as the selector Id.

    : selector create does> send-message ;

    OP: DOSEL  todo, the only difficult part is "message not understood"

Selectors can be created ahead of time, or as needed when defining methods.

### Binding

We can bind with a class and selector.

    code -bind ( sel class -- xt f )
    : bind ( sel class -- xt ) -bind abort" message not understood" ;
    : send-message ( obj sel -- )  over @ bind execute ;

Early binding:

    : 'sel  ' >body ;
    obj [ 'sel TheClass bind ] literal execute

gforth uses this:

    [bind] object method

This might not work with ivars (which should be early bound anyway??).

## Methods

Methods take an addition parameter which is the object. At the start
of the method we need to save self and set it from the stack.
The saving and restoring of self are compiled with m: and m; respectively.

m: method  ( args [obj] -- )  self >r to self  ...   r> to self ;

or like gforth:

m:  ( -- ) ... m; overrides method

The latter may be a good factorization although a bit clumsy syntatically.
And it requires that selectors are predefined. Which may not be such a bad thing.
If we mess with search order we need to make sure selectors go into FORTH.

However it is nice in that it make things like Object::addr easy

    :noname ; overrides addr
    :noname drop ; overrides init

or even
    ' nop overrides addr
    ' drop overrides init

If we do this, we should pick a better name since "override" is only meaningful
for a vtable implementation. Perhaps "implements" or "does";

    Object class Point
        selector get
        selector put
        selector print
        var x
        var y
        m: x @ y @ ;m overrides get
    end-class

In any case, we want to keep selector unique, so `selector` must first look
to see if a selector has already been define, and only if not will it
create a new one.

    : sel? ( xt -- f ) \\ is this a selector?
    : selector ( -- )
        >in @  bl word find if  dup sel? if  2drop exit  then then  drop >in !
        create ...

This is just more wordy, let's have m: take the selector name, which it can
create if needed.

    : get-selector ( -- sel ) \ find or create
        >in @  bl word find if  dup sel? if  >body nip exit  then then  drop >in !
        create here  does> send-message ;
    : selector get-selector drop ;

Do we even need selector now? Sure, we may want to send a message to an object
and later define the object or method. Think about it...but no harm.

    : m:  get-selector  this >method link, ,  $m: OP,  ] ;
    compiler
    : exitm  $;m op, ;
    : ;m  \\ exitm \\ [ ;
    forth

## Object Structure

The first cell of an object points to the class.
Next are the instance variables.
Intance variables (ivars) can be var (like a value), buffers (return their address)
or objects.

Unlike Neon, objects are referenced by the address of the class, not the
first real instance variable. So "class" is the first ivar at offset 0.

## Class structure

- class pointer (classes are objects too)
- method table (n-way linked lists)
- instance variable size (for allocating, including class pointers)
- list of instance variables (name, type/class, offset, size)
- indexed ivars (from Neon, do we need this?)
- super class

Class needs a list of object ivars so the class fields can be initialized.

Objects are initialized starting with Object and proceeding through the
parent-class chain. Thus we need a pointer to be base class.

Object is the only class without a superclass. It has a few general methods
but no instance variables other than the class pointer.

The class of all classes is Meta. Meta must be contructed by hand.
We could call this 'class'. It would have methods like new, ivar, subclass, etc.

We probly need to construct Object by hand too, at least part of it.

    create Meta
        here to this
        here , ( it's its own class )
        0 , 0 , ... n times method threads
        0 , (stuff)
        ...

        m: subclass \ clone me
        m: new
        m: ivar

    Meta class: Object
        m: init ( constructor )  ;m
        m: addr  [ 0 ivar, ] ;m
        ' nop overrides addr
        ' nop overrides init
        m: print  ." Object@" self addr .  ;m
    ;class

: class:  subclass ; \ syntactic sugar
: ;class ... ;

## Instance Variables

How do they get found?

In Neon they are found by the selector so they aren't Forth words.
Here they need to be words, but will be different for each class
and not useful outside of a class definition.

We could use a private wordlist (which we'd have to implement).
Or we could just cut it off after closing the class (like meta).
We might (probably do) want to allow a class to access the ivars of the parent classes.

If ivars are protected, they only need to be found when compiling,
so they could be added to the COMPILER vocabulary. This makes sense since
we want then to compile special primatives. If we don't they'd get compiled
as calls unless we add them to COMPILE, (I'd like to avoid that).

So they are words that push (or create code to push) their address on the stack.

var is like value so it leaves it's value, modified by TO.

Instance variable body

- offset from start of containing object
- class (0 for basic types)
- flags (if required)
- link so we can link them into the class.

runtime for buf/object:    `does> @ self + ;`
runtime for var:    `does> @ self + @ ;`

Idea: make all ivars the same, just returning their address. Then @ and !
can optimize if necessary.

    variable #ivar ( includes cell for class pointer )
    : ivalign  #ivar @ aligned #ivar ! ;
    : bytes  create  #ivar @ ,  @ivar +!  does> @ self + ;
    : var  ivalign cell bytes ;

If ivar is in COMPILER

    : ivar, ( n -- )  $xx OP, , ;
    does> @ ivar, ;

    OP: IV      push self + LIT; I += CELL; NEXT
    OP: IV@     push *(cell *)(self + LIT); I += CELL; NEXT
    OP: IV!     *(cell*)(self + LIT) = top, pop; I += CELL; NEXT

We can early bind object ivars, they can look ahead at the selector. But this
would prohibit passing in selectors as arguments at runtime.
Hmm, that's an unusual situation, so we can accomate it with a special syntax
and default to early binding ivars.

    string :: mystring
    m: method  ...   mystring print  ... m;

    mystring does> @ ivar,  'sel this bind  compile, ;

To force late binding, just do `mystring addr print`.

### Self and Super

self and super are both in the compiler wordlist and do early binding.
They compile a reference to the current object (ivar at offset 0)
followed by the xt of the method.

    : self,  ( class -- )  0 ivar,  'sel swap bind compile, ;
    : self   this self, ;
    : super  this SFA @ self, ;

Now an unadorned self doesn't work, but we can add the "addr" method to Object
and use `self addr`.

### Other ideas for self/super

self is easy, just push the object's address (self is a register).

super, we need the same address but with a diffent class. hmmmm.

One way, which would be a pain, is that objects are represented by
two items on the stack, object address and class. This makes it awkward
to store objects in variables, etc. But it does decouple the memory
for an object from its class. And we can have objects without class
pointers so they can plain-old-structures (e.g. if we got them from
an outside source).

Another way is that an object is a two-cell item, the first pointing
to the class and the second to the data. The data would normally by
right after but not necessarily so. This makes ivars very awkward because
we have to manufacture the two-cell at runtime, and to support recursion
it has to be on r-stack or heap. Not good.

So that leave a special incantation for super, which might be ok since it
is not used all the time (is that true?). Perhaps just using class binding.


    `self  super ['] get bind execute`
    `self  [ super ' get bind ] literal execute`

From objects.fs:

this [super] selector

## Example

    object class: point
        var x
        var y

        m: get   x @ y @  ;m
        m: put   y ! x !  ;m
        m: print x ? y ?  ;m
    ;class

    \ or with var as a value
    object subclass point
        var x
        var y
        m: get  x y  m;
        m: put  to y to x  m;
    end-class


    point :: p
    p get
    4 5 p put

    object class rect
        point ivar upperLeft
        point ivar lowerRight

        m: get  ( -- x y x' y' )  upperLeft get  upperRight get  m;
        m: print  upperLeft print  upperRight print ;m
    end-class

    rect new value myrect
    myrect get
    myrect delete
