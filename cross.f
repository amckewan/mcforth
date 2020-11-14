\ Cross compiler

warnings off

: TAG S" cross forth" ;

HEX

CREATE EOL 1 C, 0A C,
: H, , ;

\ Memory Access Words
CREATE IMAGE 2000 ALLOT   IMAGE 2000 ERASE
: THERE  ( taddr -- addr )   IMAGE + ;
: TC@    ( taddr -- char )   THERE C@ ;
: TC!    ( char taddr -- )   THERE C! ;

\ Support 64-bit gforth and 32-bit target
\ CELL is 4 or 8 defined on command line
CELL 1 CELLS = [IF]
: T@     ( taddr -- n )      THERE @ ;
: T!     ( n taddr -- )      THERE ! ;
[ELSE]
: T@     ( taddr -- n )      THERE UL@ ;
: T!     ( n taddr -- )      THERE L! ;
[THEN]

VARIABLE H
: HERE  ( -- taddr )   H @ ;
: ALLOT ( n -- )       H +! ;
: C,    ( char -- )    HERE TC!      1 H +! ;
: ,     ( n -- )       HERE  T!   CELL H +! ;
: S,    ( addr len -- )
   0 ?DO   COUNT C,   LOOP   DROP ;

: ALIGN  BEGIN HERE CELL 1 - AND WHILE 0 C, REPEAT ;

: TDUMP  IMAGE H @ DUMP ;

\ Output to prims.inc
: ?ERR  ABORT" file I/O error" ;

VARIABLE OUT
: OPEN   R/W CREATE-FILE ?ERR OUT ! ;
: WRITE  ( adr len -- )  OUT @ WRITE-FILE ?ERR ;
: NEWLINE   EOL COUNT WRITE ;

: `   1 PARSE WRITE  NEWLINE ;
: ``  BEGIN  REFILL 0= ABORT" missing ``"
        BL WORD COUNT S" ``" COMPARE WHILE
        SOURCE WRITE NEWLINE
      REPEAT ;

\ write decompiler information
variable seer
: open-info     w/o create-file ?err seer ! ;
: close-info    seer @ close-file ?err ;

: info ( opc -- )
    0 <# # # #> seer @ write-file ?err
    s"  OP " seer @ write-file ?err
    >in @  bl word count seer @ write-line ?err  >in ! ;

: CLOSE  OUT @ CLOSE-FILE ?ERR ;

\ save image
: SAVE-IMG
    R/W CREATE-FILE ?ERR
    DUP IMAGE HERE ROT WRITE-FILE ?ERR
    CLOSE-FILE ?ERR ;

: SAVE-INC
    OPEN  BASE @ DECIMAL
    HERE 0 DO
        I THERE 10 0 DO
            COUNT 0 <# #S #> WRITE  S" ," WRITE
        LOOP DROP
        NEWLINE
    10 +LOOP  CLOSE  BASE ! ;

: SAVE  ( -- )
    CLOSE  close-info
    CR ." Saving " BASE @ DECIMAL HERE . BASE ! ." bytes..."
    S" kernel.img" SAVE-IMG
    S" kernel.inc" SAVE-INC
    ." done" ;

S" prims.inc" OPEN
S" see.info" open-info

: ciao cr bye ;

\ **********************************************************************
\ Compiler

: +ORIGIN  ( n -- ta )  CELL * ;

VARIABLE ?CODE
: LATEST ( -- n )  ?CODE @ DUP IF TC@ ELSE INVERT THEN ;
: PATCH  ( n -- )  ?CODE @ DUP 0= ABORT" patch?" TC! ;
: OP,  ( opcode -- )  HERE ?CODE !  C, ;

: COMPILE,  ( addr -- )
    DUP TC@ 5F >  OVER 1+ TC@ 0= AND IF  TC@ OP, EXIT  THEN
    1 OP, DUP C, 8 RSHIFT C, ( le) ;

: EXIT  0 OP, ;

\ Create Headers in Target Image
CREATE CONTEXT  1 H, 9 +ORIGIN ( NULL) DUP H, ( FORTH ) H, ( COMPILER )
: FORTH     1 CONTEXT ! ; FORTH
: COMPILER  2 CONTEXT ! ;

: PRUNE  ( store here and context for target )
    HERE 2 +ORIGIN T!  CONTEXT CELL+ 2@  7 +ORIGIN T!  8 +ORIGIN T! ;

: HASH   ( voc -- thread )  CELLS CONTEXT + ;

: HEADER   ( -- )
    ALIGN  HERE  CONTEXT @ HASH  DUP @ ,  !
    BL WORD COUNT DUP ( 80 OR)  C, S,  ALIGN ;

VARIABLE STATE-T
: ?EXEC  STATE-T @ 0= ABORT" cannot execute target word!" ;

VARIABLE CSP
: !CSP  DEPTH CSP ! ;
: ?CSP  DEPTH CSP @ - ABORT" definition not finished" ;

: TARGET-CREATE   ( -- )
   >IN @ HEADER >IN !  CREATE  HERE H,
   DOES>  ?EXEC  @ COMPILE, ;

: H. . ;
: '-T  ' >BODY @ ;
: HAS ( n -- )  '-T  SWAP +ORIGIN T! ;

\ Generate primatives
: ?COMMENT  ( allow Forth comment after OP: etc. )
    >IN @  BL WORD COUNT S" (" COMPARE
    IF  >IN !  ELSE  DROP  [COMPILE] (  THEN ;

: C-COMMENT  S" /* " WRITE  BL WORD COUNT WRITE  S"  */ " WRITE ;
VARIABLE OP  ( next opcode )
: OP!  OP ! ;
: OP:  ( output opcode case statement )
    OP @ FF > ABORT" opcodes exhausted"
    OP @ info
    C-COMMENT  S" case 0x" WRITE  OP @ 0 <# # # #> WRITE  S" : " WRITE
    ?COMMENT ` ( copy rest of line )  1 OP +! ;

: (PRIM)   OP @ OP,  EXIT  OP: ;
: PRIM   >IN @ HEADER        >IN ! (PRIM) ;  ( in target only )
: CODE   >IN @ TARGET-CREATE >IN ! (PRIM) ;  ( in host and target)

: BINARY  ( op -- )
    CREATE H,  DOES> ?EXEC @ OP, ;

\ Target Literals
: LITERAL  ( n -- )  ?EXEC  8 OP,  , ;
: $   BL WORD NUMBER DROP LITERAL ;

\ Define Meta Branching Constructs
: ?CONDITION  INVERT ABORT" unbalanced" ;
: MARK      ( -- here )     ?EXEC  HERE  0 ?CODE ! ;
: >MARK     ( -- f addr )   TRUE  MARK   0 C, ;
: >RESOLVE  ( f addr -- )   MARK  OVER -  SWAP TC!   ?CONDITION ;
: <MARK     ( -- f addr )   TRUE  MARK ;
: <RESOLVE  ( f addr -- )   MARK  - C,   ?CONDITION ;

: CONDITION  ( todo optimizer )
    58 OP, ;

: NOT  ( invert last conditional op )  ?EXEC  LATEST 70 78 WITHIN
0 AND
    IF  LATEST 8 XOR PATCH  ELSE  70 OP, ( 0= )  THEN ;

: IF        CONDITION  >MARK ;
: THEN      >RESOLVE ;
: ELSE      3 OP,  >MARK  2SWAP >RESOLVE ;
: BEGIN     <MARK ;
: UNTIL     CONDITION  <RESOLVE ;
: AGAIN     3 OP,  <RESOLVE ;
: WHILE     IF  2SWAP ;
: REPEAT    AGAIN  THEN ;

: DO        4 OP,  >MARK  <MARK ;
: ?DO       5 OP,  >MARK  <MARK ;
: LOOP      6 OP,  <RESOLVE  >RESOLVE ;
: +LOOP     7 OP,  <RESOLVE  >RESOLVE ;

\ Compile Strings into the Target
: C"  HERE  [CHAR] " PARSE S,  0 C, ; \ c-style string
: ,"  ?EXEC  [CHAR] " PARSE  DUP C,  S,  0 ?CODE ! ;

: S"      0A OP,  ," ;
: ."      0B OP,  ," ;
: ABORT"  0C OP,  ," ;

: \\ ;
: { ;
: } ;
: forget ;
\ : CELL+ CELL + ;
\ : CELLS CELL * ;

\ Defining Words
: CONSTANT  TARGET-CREATE  10 OP, ALIGN   , ;
: VARIABLE  TARGET-CREATE  11 OP, ALIGN 0 , ;

: BUFFER ( n <name> -- )  ALIGN  HERE  SWAP ALLOT  CONSTANT ;
: TAG  HERE  TAG DUP C, S,  CONSTANT ;

: [   0 STATE-T ! ;
: ]  -1 STATE-T ! ;

: T:  HEADER   0 ?CODE !  ] ;  \ to create words with no host header

: ;_  [COMPILE] ; ; IMMEDIATE \ concession
: ;   ?CSP EXIT [ ;
: :   TARGET-CREATE  0 ?CODE !  !CSP ] ;_

include kernel.f

PRUNE
SAVE
