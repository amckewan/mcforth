( Objects )

(
Class structure
  0 --> class (so classes can be objects too
1-7 --> method table (hashed lists
  8 --> instance variable list
  9 --> datalen of named ivars
 10 --> width of indexed area
 11 --> superclass pointer
 12 --> class tag ??
)

: MFA      CELL+   ;    \ method dictionary
: IFA    8 CELLS + ;    \ ivar dict Latest field
: DFA    9 CELLS + ;    \ datalen of named ivars
: XFA   10 CELLS + ;    \ width of indexed area, <= 0 if not indexed
: SFA   11 CELLS + ;    \ superclass ptr field
: TAG   12 CELLS + ;    \ class tag field

\ =====================================================================
\ Objects have a pointer to their class stored in the first cell of
\ their pfa. When they execute, they return the address of the cell
\ following the class pointer, which is location of the first named
\ instance variable.
\
\ Object structure: | ^class | named ivars |
\
\ If an object is embedded...
\
\ | class-offset | -1 | named ivars |
\ | -class-offset | named ivars
\

\ If an object is indexed, an indexed header appears after the data area.
\ This header consists of a cell containing the number of elements.
\ The indexed data follows this header.
\
\ Indexed object:   | ^class | named ivars | #elems | indexed ivars |
\ =====================================================================

: (Obj)  ( -- )  CREATE  DOES> ;

\ =====================================================================
\ Methods are stored in an 7-way linked-list from the MFA field.
\ Each method is identified by a 32-bit hash of the name.
\
\ Method Structure:
\   0   link to previous method
\   1   selector
\   2   method xt (code starts here), contains (;m)
\
\ =====================================================================

\ MFA finds the top of the method link for a given selector.

: MFA  ( SelID ^Class -- SelID MFA )  CELL+  OVER 7 UMOD CELLS + ;

: (find)  ( sel list -- xt f )
    begin  @ dup while
        2dup cell+ @ = if  2 cells +  nip true exit  then
    repeat ;

: huh  1 abort" message not understood" ;

: findm  ( sel class -- xt f )
    mfa (find) 0= if  drop ['] huh  then ;

: findm  ( sel list -- xt f )
    mfa begin  @ dup while
        2dup cell+ @ = if  2 cells +  nip exit  then
    repeat
    drop ['] huh ;

\S
\ ============================

: hash ( str len -- u )
    5381 -rot bounds ?do  33 * i c@ +  loop ;


: find-method ( sel obj -- xt ) ;

    2drop [‘] huh ;

: send ( obj sel -- )  over find-method execute ;

: selector ( <name> -- )
    >in @  parse-name hash  swap >in !
    create , does> @ send ;

0 value self
: (:m)  ( obj -- )  r>  self >r  >r  to self ;
: (;m)  ( -- )      r>  r> to self  >r ;

: :m  ( link )  parse-name hash ,  ] ;
: ;m  \\ exit \\ [ ;


\S
class vector
public
:m clear ;m
private
;class
