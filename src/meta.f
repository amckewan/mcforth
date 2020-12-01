( metacompiler )

: TAG S" Hello" ;

VARIABLE H'  HEX 8000 ,

: +ORIGIN  ( n -- ta )  CELL * ;

\ ========== Save target ==========

: ?ERR  ABORT" file I/O error" ;

VARIABLE OUT
: OPEN   R/W CREATE-FILE ?ERR  OUT ! ;
: WRITE  OUT @ WRITE-FILE ?ERR ;
: CLOSE  OUT @ CLOSE-FILE ?ERR ;

CREATE EOL 1 C, 0A C,
: NEWLINE   EOL COUNT WRITE ;

: `   1 PARSE WRITE  NEWLINE ;
: ``  BEGIN  REFILL 0= ABORT" missing ``"
        PARSE-NAME S" ``" COMPARE WHILE
        SOURCE WRITE  NEWLINE
      REPEAT ;

( write decompiler information )
VARIABLE SEER
: OPEN-INFO   R/W CREATE-FILE ?ERR SEER ! ;
: CLOSE-INFO  SEER @ CLOSE-FILE ?ERR ;
: WRITE-INFO  SEER @ WRITE-FILE ?ERR ;

: INFO ( opc -- )
    0 <# # # #> WRITE-INFO  S"  OP " WRITE-INFO
    >IN @  PARSE-NAME WRITE-INFO  EOL COUNT WRITE-INFO  >IN ! ;

( save dictionary image )
: SAVE-IMAGE ( for hexdump )
    OPEN  H' 2@ OVER - WRITE  CLOSE ;
: SAVE-DICT ( for #include )
    H' @ 10 ERASE ( for diff )
    OPEN  BASE @ DECIMAL
    H' 2@ SWAP DO
        I  10 0 DO  COUNT 0 <# #S #> WRITE  S" ," WRITE  LOOP DROP
        NEWLINE
    10 +LOOP  CLOSE  BASE ! ;

: SAVE  ( -- )
    CLOSE  CLOSE-INFO
    CR ." Saving " BASE @ DECIMAL H' 2@ SWAP - . BASE ! ." bytes..."
    S" kernel.img" SAVE-IMAGE
    S" kernel.inc" SAVE-DICT
    ." done " ;

S" prims.inc" OPEN
S" see.info"  OPEN-INFO


\ ========== Generate Primatives ==========

: ?COMMENT  ( allow Forth comment after OP: or CODE )
    >IN @  PARSE-NAME S" (" COMPARE
    IF  >IN !  ELSE  DROP  \\ (  THEN ;

: C-COMMENT  S" /* " WRITE  PARSE-NAME WRITE  S"  */ " WRITE ;

VARIABLE OP  ( next opcode )
: OP!  OP ! ;
: OP:  ( output opcode case statement )
    OP @ FF > ABORT" opcodes exhausted"
    OP @ info
    C-COMMENT  S" case 0x" WRITE  OP @ 0 <# # # #> WRITE  S" : " WRITE
    ?COMMENT ` ( copy rest of line )  1 OP +! ;
: ---  1 OP +! ;

: CODE  >IN @ HEADER >IN !  OP @ OP,  \\ EXIT  OP: ;

( misc. stuff )
: C" ( -- a )  HERE dA @ -  [CHAR] " PARSE S,  0 C, ; \ null-terminated string

: BUFFER ( n <name> -- )  ALIGN  HERE dA @ - SWAP
    HERE OVER 0 FILL  ALLOT  CONSTANT ;

: TAG  HERE dA @ -  TAG DUP C, S,  CONSTANT ;

: HAS ( a -- )  ' dA @ -  SWAP +ORIGIN dA @ +  ! ;

\ ========== Target Compiler ==========

: ciao CR BYE ;

MARKER EMPTY
: THERE  HERE dA @ - ;

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
   6 +ORIGIN 8000 + XOR  dA @ -  SWAP !  DUP @  SWAP dA @ +  ! ;
: PRUNE   { CONTEXT CELL+  DUP CLIP  CELL+ CLIP
    THERE 2 +ORIGIN 8000 + !  { EMPTY ;

\ for compatibility with cross.fs
COMPILER : $ ; FORTH
: T: : ;
: forget SMUDGE ;
: ,A  dA @ - , ;

HEX  8000 1000 0 FILL  8000 H' !
{ include ./kernel.f }
PRUNE
SAVE
