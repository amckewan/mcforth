( Objects )

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

: (Obj)  ( -- )  CREATE  DOES> ;

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

0 [IF]
    xt find_method(cell methods, cell selector) {
        cell link = methods + (selector & CELLS(THREADS - 1));
        while ((link = AT(link)) {
            if (AT(link + CELL) == selector) {
                return link + 2 * CELL;
            }
        }
        return 0;
    }
[THEN]

: hash  ( sel class -- sel list )  cell+ over 7 cells and + ;
: link,  ( var -- )  align here  over @ ,  swap ! ;

: -bind ( sel class -- xt f )
    cell+ over 7 cells and +
    begin @ dup while
        2dup cell+ @ = if  2 cells +  nip false exit  then
    repeat invert ;

: bind ( sel class -- xt ) -bind abort" message not understood" ;
: send-message ( obj sel -- )  over @ bind execute ;

: selector create does> send-message ;

0 value self
: (m:)  ( obj -- )  r>  self >r  >r  to self ;
: (;m)  ( -- )      r>  r> to self  >r ;

create Object  here class-size dup allot erase

object value ^class

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
m: mprint ." hey " self . depth . ;m


create o Object ,

\ o print

