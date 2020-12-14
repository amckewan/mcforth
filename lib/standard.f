( some standard words for passing the tests )

: DABS ;

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
