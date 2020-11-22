\ Objects
\ Adapted from Neon model
\ Copyright (c) 2020 Andrew McKewan

forth decimal

\ =====================================================================
\ Class structure
\ =====================================================================

8 constant #threads \ todo use this

\ Offsets from the XT of the class

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

make-selector init

: sel? ( xt -- f )  @ [ ' init @ ] literal = ;

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

: method  selector  ^class hash link,  ,  ] ;

: enter  r>  ^self >r  >r  to ^self ;
: endm   r>drop  r> to ^self ;

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
\ Instance Variables
\
\ 0 --> offset
\ 1 --> class (or 0)
\
\ Instance variables are compiler words
\ =====================================================================

: classes 4 context ! ;

: ivalign  ^class DFA dup @ aligned swap ! ;

: do-plain-ivar  ( offset -- )
    ^self + ;

: doivar  ( offset -- )
    \ cr ." iv@" dup .
    ^self + ;

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

: (ivar)  classes (ivar) forth ;

: bytes  ( n -- )  0 swap (ivar) ;

: make-ivar ( class <name> -- )
    ivalign dup DFA @ (ivar) ;

classes
: self   postpone ^self  'sel ^class       bind compile, ;
: super  postpone ^self  'sel ^class SFA @ bind compile, ;
forth

\ =====================================================================
\ Build class instances
\ =====================================================================

: make-object ( class <name> -- )
    create  dup ,  here swap DFA @ dup allot erase
    \ todo: initialize ivars
    does> cell+ ;

: var  ^class if  make-ivar  else  make-object  then ;

\ : new ...

\ =====================================================================
\ Create classes
\ =====================================================================

: subclass  ( class <name> -- )
    create here
    dup to ^class
    2dup class-size dup allot move
    SFA ! ;

: end-class
    \ todo: detach ivar vocabulary
    0 to ^class ;

create Object   here to ^class
                here class-size dup allot erase

    method init     drop ;
    method print    ." Object " . ;
end-class

: class  object subclass ; ( shorthand )
