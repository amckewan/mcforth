( Objects )

forth decimal

((
Note on the "addresses" for the entities:
    Class:      XT
    Selector:   XT
    Object:     first instance variable (after class pointer)
))

\ =====================================================================
\ Class structure
\ =====================================================================

8 constant #threads \ todo use this

\ Offsets from the XT of the class

: MFA      CELL+   ;    \ method table
: IFA    9 CELLS + ;    \ ivar vocabulary
: DFA   10 CELLS + ;    \ datalen of named ivars
: XFA   11 CELLS + ;    \ width of indexed ivars
: SFA   12 CELLS + ;    \ superclass ptr field

12 cells constant class-size ( not including xt )

0 value ^class

\ =====================================================================
\ Methods are stored in an 8-way linked-list from the MFA field.
\
\ Method Structure:
\   0   link to previous method
\   1   selector
\   2   method code starts here
\
\ =====================================================================

: hash  ( sel class -- sel list )  MFA over 7 cells and + ;

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

((
Selectors must be unique, so only create then if they don't already exist.

TODO: Consider using the XT for the selector so a simple ' will do
for early binding.

    ' print ' object bind execute
))

: make-selector ( -- sel )
    \ header here $xx ,
    create here cell -  does> cell - send-message ;

make-selector print @

: sel? ( xt -- f )  @ literal = ;

: selector ( -- sel )
    >in @  bl word find if  dup sel? if  nip exit  then then  drop
    >in !  make-selector ;

\ The check is not really necessary, but may prevent
\ a later "not understood" error.
: 'sel  ( -- sel )  ' dup sel? 0= abort" not a selector" ;

\ =====================================================================
\ Methods
\ =====================================================================

0 value ^self

: link,  ( var -- )  align here  over @ ,  swap ! ;

: method  selector  ^class MFA hash link,  ,  ] ;

: enter  r>  ^self >r  >r  to ^self ;
: exitm  r>drop  r> to ^self ;

: m:    method  postpone enter ;
compiler
: ;m    postpone exitm  postpone [ ;
forth

\ =====================================================================
\ Class Object
\
\ Object is the base class for all objects.
\ We need to construct some of this by hand.
\
\ =====================================================================

: make-object ( class <name> -- )
    create  dup ,  here swap DFA @ dup allot erase
    \ todo: initialize ivars
    does> cell+ ;

: make-ivar 1 abort" todo" ;

: do-class  does> make-object ;

create Object   do-class
                here cell - to ^class
                here class-size dup allot erase

method init     drop ;
method addr     ;
method print    ." Object@" . ;

: subclass  ( class <name> -- )
    create do-class
    here cell - to ^class
    dup here class-size dup allot move
    ^class SFA ! ;

: class ['] object subclass ;

: end-class
    \ detach ivar dictionary
    0 to ^class ;


: classes 4 context ! ;

((
class point

end-class

class enhanced-point
' point subclass enhanced-point

))


\ =====================================================================
\ Testing
\ =====================================================================

Object o

\ o print

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

' object subclass point
var x
var y

m: print x @ 0 .r ." @" y ? ;m

point p 3 , 4 ,

\S
create p point , 3 , 4 ,

object subclass rect
point 3 cells (ivar) p1
point 3 cells (ivar) p2
m: print  p1 print  p2 print  ;m

create r rect , point , 1 , 2 , point , 3 , 4 ,

