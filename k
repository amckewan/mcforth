( target compiler )
: \\  0 ?CODE ! ;

VARIABLE H'  HEX 2000 ,
         : {   dA @  HERE  H' 2@ H !  dA !  H' 2! ;
         : }   { ;
COMPILER : }   H' @ ,  \\ PREVIOUS  80 XOR  SWAP C!  { ;
FORTH    \ : forget   SMUDGE ;
         \ : RECOVER   -2 ALLOT ;
         \ : ADR>CALL ( a - n)   dA @ - U2/ ;

: SCAN ( a - a)   @ BEGIN  DUP 1 2000 WITHIN WHILE  @ REPEAT ;
: SCAN ( a - a)   BEGIN  @  DUP 1 2000 WITHIN NOT UNTIL ;
: TRIM ( a a - a)   DUP >R  dA @ -  SWAP !  R>
   DUP CELL+  DUP C@  DF AND  SWAP C! ;
: CLIP ( a)   DUP BEGIN  DUP SCAN  DUP WHILE  TRIM  REPEAT
   2212 XOR  dA @ -  SWAP !  @ , ;
: PRUNE   { CONTEXT CELL+  DUP CLIP  CELL+ CLIP {
   20 0 2212 2!  EMPTY ;

