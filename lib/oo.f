( Objects )

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
\ Methods are stored in an 8-way linked-list from the MFA field.
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

((
Selectors must be unique, so only create then if they don't already exist.

TODO: Consider using the XT for the selector so a simple ' will do
for early binding.

    ' print ' object bind execute
))

: make-selector   create does> send-message ;

make-selector init

: sel? ( xt -- f )  @ [ ' init @ ] literal = ;

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
\ Instance Variables
\
\ 0 --> offset
\ 1 --> class (or 0)
\
\ Instance variables are immediate words
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

: var  ivalign cell bytes ;

: make-ivar ( class <name> -- )
    ivalign dup DFA @ (ivar) ;

classes
: self   postpone ^self  'sel ^class       bind compile, ;
: super  postpone ^self  'sel ^class SFA @ bind compile, ;
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

: do-class  does> ^class if  make-ivar  else  make-object  then ;

create Object   do-class
                here to ^class
                here class-size dup allot erase

method init     drop ;
method print    ." Object@" . ;

: subclass  ( class-xt <name> -- )
    create do-class  >body here
    dup to ^class
    2dup class-size dup allot move
    SFA ! ;

: class ['] object subclass ;

: end-class
    \ detach ivar dictionary
    0 to ^class ;



\ =====================================================================
\ Testing
\ =====================================================================

Object o

\ o print


' object subclass point
var y
var x

method get 2@ ;
method put 2! ;

m: print x @ 0 .r '@' emit y ? ;m

end-class

point p


class rect
    point p1
    point p2
    m: print  p1 print  p2 print  ;m
    m: put p2 put  p1 put ;m
    m: setupr ( pt -- )  get p1 put ;m
end-class

rect r

' rect subclass crect
    var color
    m: print  super print  color ? ;m
    m: setcolor  color ! ;m
end-class

crect rr

\S
create p point , 3 , 4 ,

object subclass rect
point 3 cells (ivar) p1
point 3 cells (ivar) p2
m: print  p1 print  p2 print  ;m

create r rect , point , 1 , 2 , point , 3 , 4 ,

