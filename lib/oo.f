( Objects )

forth decimal

(( Class structure

      0 --> class (so classes can be objects too)
    1-8 --> method table (hashed lists)
      9 --> instance variable list (vocabulary)
     10 --> object length (including class pointer)
     11 --> superclass pointer
))

: MFA      CELL+   ;    \ method dictionary
: IFA    9 CELLS + ;    \ ivar dict Latest field
: DFA   10 CELLS + ;    \ datalen of named ivars
: SFA   11 CELLS + ;    \ superclass ptr field

12 cells constant class-size

\ =====================================================================
\ Methods are stored in an 8-way linked-list from the MFA field.
\ Each method is identified by the PFA of the selector
\
\ Method Structure:
\   0   link to previous method
\   1   selector
\   2   method xt (code starts here), contains (m:)
\
\ =====================================================================

: hash  ( sel class -- sel list )  cell+ over 7 cells and + ;

: -bind ( sel class -- xt f )
    hash begin @ dup while
      2dup cell+ @ = if  2 cells +  nip false exit  then
    repeat invert ;

: bind ( sel class -- xt ) -bind abort" message not understood" ;
: send-message ( obj sel -- )  over @ bind execute ;

: selector create does> send-message ;

: link,  ( var -- )  align here  over @ ,  swap ! ;


0 value ^self

: (m:)  ( obj -- )  r>  ^self >r  >r  to ^self ;
: (;m)  ( -- )      r>  r> to ^self  >r ;

create Object  here class-size dup allot erase

object value ^class

cell object dfa !

Selector print

' print >body object hash link, ,
] ." Object@" . exit [

: get-selector ( -- sel ) ' >body ;

: m:
    get-selector ^class hash link, ,
    postpone (m:) ] ;

compiler
: ;m  postpone (;m)  \\ exit \\ [ ;
forth


Selector mprint
m: mprint ." hey " ^self . depth . ;m


create o Object ,

\ o print


: subclass ( class -- )
    create  here to ^class  class-size allot
    dup ^class class-size move  ^class SFA !
    ;

: ;class ;

: class 4 context ! ;

(( Instance variables

    0 --> link
    0 --> offset
    0 --> class
))

: ivalign  ^class DFA dup @ aligned swap ! ;

: doivar  ( offset -- )
    \ cr ." iv@" dup .
    ^self + ;

: (ivar)  ( class size -- )
    create
    ^class IFA link,
    \ cr ." ivar @ " ^class DFA ?
    ^class DFA dup @ , +!
    ( class ) ,
    does> cell+ @ \\ literal  postpone doivar ;

: (ivar)  class (ivar) forth ;

: bytes  ( n -- )  0 swap (ivar) ;

: var  ivalign cell bytes ;

object subclass point
var x
var y

m: print ." I am a point " x ? y ? ;m

create p point , 3 , 4 ,

object subclass rect
point 3 cells (ivar) p1
point 3 cells (ivar) p2
m: print  p1 print  p2 print  ;m

create r rect , point , 1 , 2 , point , 3 , 4 ,

