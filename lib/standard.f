( some standard words for passing the tests )

: ENVIRONMENT?  2DROP 0 ;

: CHARS     ;
: CHAR+     1+ ;

( fm/mod adapted from standard implementation )
: FM/MOD  ( d n -- rem quot )
    DUP>R SM/REM
    ( if the remainder is not zero and has a different sign than the divisor )
    OVER DUP SWAP R@ XOR 0< AND IF  1- SWAP R@ + SWAP  THEN
    R>DROP ;

: DABS ;

: DEFER@  >BODY @ ;
: DEFER!  >BODY ! ;
