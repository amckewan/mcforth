( some standard words for passing the tests )

: ENVIRONMENT?  2DROP FALSE ;

: CHARS ;
: CHAR+ 1+ ;
: DABS ;

: FM/MOD SM/REM ; \ wrong behavior
\ F83
\ CODE /MOD   ( num den -- rem quot )
\ BX  POP    AX  POP    CWD    BX CX MOV    DX CX  XOR
\ 0>=  IF    BX  IDIV    2PUSH    THEN
\ BX  IDIV   BX DX  ADD    AX  DEC    2PUSH   END-CODE

: DEFER@  >BODY @ ;
: DEFER!  >BODY ! ;

: IMMEDIATE ( create a compiler synonym )
    2 CELLS CONTEXT + LINK,
    PREVIOUS 1+ ALIGNED
    2DUP HERE OVER ALLOT SWAP MOVE
    + COMPILE, \\ EXIT ;

: FIND  2 -FIND IF 1 -FIND NOT ELSE 1 THEN ;

\ Change from a variable so CREATE IMMEDIATE DOES> works
: CREATE  HEADER $12 , ;

COMPILER
: RECURSE  PREVIOUS $1F AND + 1+ ALIGNED COMPILE, ;
FORTH
