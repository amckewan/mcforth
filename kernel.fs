( target compiler )
MARKER EMPTY
: \\  0 ?CODE ! ;

VARIABLE H'  HEX 8000 ,
         : {   dA @  HERE  H' 2@ H !  dA !  H' 2! ;
         : }   { ;
COMPILER : }   H' @ ,  PREVIOUS  80 XOR  SWAP C!  { ;
FORTH    \ : forget   SMUDGE ;
         \ : RECOVER   -2 ALLOT ;
         \ : ADR>CALL ( a - n)   dA @ - U2/ ;

: SCAN ( a - a)   BEGIN  @  DUP 1 8000 WITHIN NOT UNTIL ;
: TRIM ( a a - a)   DUP >R  dA @ -  SWAP !  R>
   DUP CELL+  DUP C@  DF AND  SWAP C! ;
: CLIP ( a)   DUP BEGIN  DUP SCAN  DUP WHILE  TRIM  REPEAT
   8024 XOR  dA @ -  SWAP !  DUP @  SWAP dA @ +  ! ;
: PRUNE   { CONTEXT CELL+  DUP CLIP  CELL+ CLIP  HERE 8008 !  { EMPTY ;

HEX  8000 1000 0 FILL  8000 H' !
{
( COLD )  0 ,  ( WARM ) 0 ,  ( H ) 0 ,  ( BASE ) #10 ,
( STATE ) 0 ,  ( 'IN )  0 ,
( CONTEXT ) 1 , 0 , 0 ,  ( NULL ) 0 , 0 , 8009 , ( NOP R>DROP EXIT )

: x ;
: y ;
: z ;
}

