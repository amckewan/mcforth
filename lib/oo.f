( Objects )

forth decimal

\ =====================================================================
\ Class structure
\ =====================================================================

8 constant #threads \ todo use this

: MFA              ;    \ method table
: IFA    8 CELLS + ;    \ ivar vocabulary
: DFA    9 CELLS + ;    \ datalen of named ivars
: XFA   10 CELLS + ;    \ width of indexed ivars
: SFA   11 CELLS + ;    \ superclass ptr field

12 cells constant class-size

0 value ^class

: make-object ( class <name> -- )
    create ,  does> cell+ ;

\ =====================================================================
\ Methods are stored in an 8-way linked-list from the MFA field.
\ Each method is identified by the PFA of the selector
\
\ Method Structure:
\   0   link to previous method
\   1   selector
\   2   method code starts here
\
\ =====================================================================

: hash  ( sel class -- sel list )  MFA over 7 cells and + ;

: -bind ( sel class -- xt f )
    hash begin @ dup while
      2dup cell+ @ = if  2 cells +  nip false exit  then
    repeat invert ;

: bind ( sel class -- xt ) -bind abort" message not understood" ;

: send-message ( obj sel -- )  over @ bind execute ;

\ =====================================================================
\ Selectors
\ =====================================================================

((
Selectors must be unique, so only create then if they don't already exist.

TODO: Consider using the XT for the selector so a simple ' will do
for early binding.

    ' print ' object bind execute
))

: make-selector  create  does> send-message ;

make-selector print

: sel? ( xt -- f )  @ ['] print @ = ;

: selector ( -- sel )
    >in @  bl word find if  dup sel? if  >body nip exit  then then  drop
    >in !  make-selector here ;

\ The check is not really necessary, but may prevent
\ a later "not understood" error.
: 'sel  ( -- sel )  ' dup sel? 0= abort" not a selector" >body ;

\ =====================================================================
\ Methods
\ =====================================================================

0 value ^self

: link,  ( var -- )  align here  over @ ,  swap ! ;

: method  selector  ^class hash link,  ,  ] ;

: enter  r>  ^self >r  >r  to ^self ;
: exitm  r>drop  r> to ^self ;

: m:    method  postpone enter ;
compiler
: ;m    postpone exitm  postpone [ ;
forth

\ =====================================================================
\ Class Object
\ =====================================================================

\ Object is the base class for all objects.
\ We need to construct some of this by hand.

create Object  here class-size dup allot erase

object to ^class

cell ^class dfa !

method init     drop ;
method addr     ;
method print    ." Object@" . ;




\ =====================================================================
\ Testing
\ =====================================================================


create o Object ,

\ o print


: subclass ( class -- )
    create  here to ^class  class-size allot
    dup ^class class-size move  ^class SFA !
    ;

: ;class ;

: classes 4 context ! ;

(( Instance variables

    0 --> link (don't need use vocab link)

    0 --> offset
    0 --> class
))

: ivalign  ^class DFA dup @ aligned swap ! ;

: doivar  ( offset -- )
    \ cr ." iv@" dup .
    ^self + ;

: (ivar)  ( class size -- )
    create
\    ^class IFA link,
    \ cr ." ivar @ " ^class DFA ?
    ^class DFA dup @ , +!
    ( class ) ,
    does> @ \\ literal  postpone doivar ;

: (ivar)  classes (ivar) forth ;

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

