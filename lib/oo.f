\ Object-oriented extensions
\ Adapted from the Neon model
\ Copyright (c) 2020 Andrew McKewan

forth decimal

internal

\ =====================================================================
\ Class structure
\ =====================================================================

\ 8 constant #threads \ todo use this

\ A class has the following fields. The first 8 cells are the
\ method table.

: IFA    8 CELLS + ;    \ ivar vocabulary
: DFA    9 CELLS + ;    \ datalen of named ivars
: XFA   10 CELLS + ;    \ width of indexed ivars
: SFA   11 CELLS + ;    \ superclass ptr field

12 cells constant class-size

0 value ^class

\ =====================================================================
\ Methods are stored in an 8-way linked-list.
\
\ Method Structure:
\   0   link to previous method
\   1   selector
\   2   method code starts here
\
\ =====================================================================

: hash  ( sel class -- sel list )  over 7 cells and + ;

: bind? ( sel class -- xt f )
    hash begin @ dup while
      2dup cell+ @ = if  2 cells +  swap exit  then
    repeat ;

: bind ( sel class -- xt ) bind? not abort" message not understood" ;

: >class ( obj -- class )  cell - @ ;

: send-message ( obj sel -- )  over >class bind execute ;

\ =====================================================================
\ Selectors
\ =====================================================================

: make-selector   create does> send-message ;

variable 'init

: sel? ( xt -- f )  @ 'init @ @ = ;

\ Selectors must be unique, so only create then if they don't already exist.
: selector ( -- sel )
    >in @  1 -' not if  dup sel? if  >body nip exit  then then  drop
    >in !  make-selector here ;

\ The check is not really necessary, but may prevent
\ a later "not understood" error.
: 'sel  ( -- sel )  ' dup sel? not abort" not a selector" >body ;

\ =====================================================================
\ Methods
\ =====================================================================

0 value ^self

: enter  r>  ^self >r  >r  to ^self ;
: endm   r>drop  r> to ^self ;

\ =====================================================================
\ Instance Variables
\
\ 0 --> offset
\ 1 --> class (or 0)
\
\ =====================================================================

\ Instance variables are compiler words in vocabulary 4.
: ivars   4 context ! ;

: init-ivars ( class -- )
    IFA @ context 4 cells + ! ;

: doivar  ( offset -- )  ^self + ;

: (ivar)  ( class size -- )
    create
        \ cr ." ivar @ " ^class DFA ?
        ^class DFA dup @ , +!
        ( class ) ,
    does>
        dup @ \\ literal  postpone doivar
        cell+ @ ( class ) ?dup if
            'sel swap bind compile,
        then ;

: (ivar)  ivars (ivar) forth ;

: ivalign  ^class DFA dup @ aligned swap ! ;

: make-ivar ( class <name> -- )
    ivalign dup DFA @ (ivar) ;

\ =====================================================================
\ Object initialization
\
\ 0. Set class pointer
\ 1. Clear all instance data to 0
\ 2. Call the init method of all instance variables (do we need this?)
\ 3. Send init message
\ =====================================================================

: init-object  ( [args] class object-base -- object )
    2dup ! cell+ swap
    2dup DFA @ erase
    \ ...init ivars if needed...
    drop dup>r 'init @ execute r> ;

: make-object ( class <name> -- )
    create  here  over DFA @ cell+ allot  init-object drop
    does> cell+ ;

\ =====================================================================
\ Public interface
\ =====================================================================

external

make-selector init  ' init 'init !

ivars
: self   postpone ^self  'sel ^class       bind compile, ;
: super  postpone ^self  'sel ^class SFA @ bind compile, ;
forth

: bind bind ;
: ivalign ivalign ;

: bytes  ( n -- )  0 swap (ivar) ;

: var  ^class if  make-ivar  else  make-object  then ;

: new  ( class -- object )
    dup DFA @ cell+ allocate abort" alloc?"  init-object ;

: subclass  ( class <name> -- )
    create here
    dup to ^class
    2dup class-size dup allot move
    over init-ivars
    SFA ! ;

: end-class
    [ context 4 cells + dup @ ] literal literal
    dup @ ^class IFA ! !
    0 to ^class ;

: method  selector  ^class hash link,  ,  ] ;

defined init-locals [IF]
: m:    method  postpone enter  init-locals ;
compiler
: exitm end-locals  postpone endm ;
[ELSE]
: m:    method  postpone enter ;
compiler
: exitm postpone endm ;
[THEN]
: ;m    \\ exitm  \\ [ ;
forth

\ =====================================================================
\ Define class Object
\ =====================================================================

create Object   here to ^class
                here class-size dup allot erase

    method init     drop ;
    method print    ." Object " . ;
end-class

: class  object subclass ; ( shorthand )

module
