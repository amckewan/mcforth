( target compiler )

VARIABLE H'  HEX 2000 ,
         : {   dA @  HERE  H' 2@ H !  dA !  H' 2! ;
         : }   { ;
COMPILER : }   H' @  2 *  ,A  \\ PREVIOUS  80 XOR  SWAP C!  { ;
FORTH    : forget   SMUDGE ;
         : RECOVER   -2 ALLOT ;
         : ADR>CALL ( a - n)   dA @ - U2/ ;

: SCAN ( a - a)   @ BEGIN  DUP 1 2000 WITHIN WHILE  @ REPEAT ;
: TRIM ( a a - a)   DUP >R  dA @ -  SWAP !  R>
   DUP 2 +  DUP C@  DF AND  SWAP C! ;
: CLIP ( a)   DUP BEGIN  DUP SCAN  DUP WHILE  TRIM  REPEAT
   2212 XOR  dA @ -  SWAP !  @ , ;
: PRUNE   { CONTEXT 4 -  DUP CLIP  2 + CLIP {
   20 0 2212 2!  EMPTY ;

