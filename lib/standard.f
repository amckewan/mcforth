( some standard words for passing the tests )

: ENVIRONMENT?  2DROP FALSE ;

: CHARS ;
: CHAR+ 1+ ;
: DABS ;

( fm/mod adapted from standard implementation )
: FM/MOD  ( d n -- rem quot )
    DUP>R SM/REM
    ( if the remainder is not zero and has a different sign than the divisor )
    OVER DUP SWAP R@ XOR 0< AND IF
        1- SWAP R@ + SWAP
    THEN  R>DROP ;

: DEFER@  >BODY @ ;
: DEFER!  >BODY ! ;

\ : xIMMEDIATE ( create a compiler synonym )
\     2 CELLS CONTEXT + LINK,
\     PREVIOUS 1+ ALIGNED
\     2DUP HERE OVER ALLOT SWAP MOVE
\     + COMPILE, \\ EXIT ;

\ : xFIND  2 -FIND IF 1 -FIND NOT ELSE 1 THEN ;

\ Change from a variable so CREATE IMMEDIATE DOES> works
\ : CREATE  HEADER $12 , ;
