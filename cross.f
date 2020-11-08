\ Cross compiler

: TAG S" cross forth" ;

HEX
\ Support 64-bit gforth host
4 CONSTANT CELL-T
: CELL+-T CELL-T + ;
: CELLS-T CELL-T * ;
: CELL@ UL@ ; \ gforth
: CELL! L! ; \ gforth
\ : CELL, HERE CELL-T ALLOT CELL! ;
warnings off


\ Memory Access Words
CREATE TARGET-IMAGE 2000 ALLOT   TARGET-IMAGE 2000 ERASE
VARIABLE DP-T
: THERE   ( taddr -- addr )   TARGET-IMAGE + ;
: C@-T    ( taddr -- char )   THERE C@ ;
: @-T     ( taddr -- n )      THERE CELL@ ;
: C!-T    ( char taddr -- )   THERE C! ;
: W!-T    ( w taddr -- )      THERE OVER 8 RSHIFT OVER 1+ C! C! ; ( le )
: !-T     ( n taddr -- )      THERE CELL! ;
: HERE-T  ( -- taddr )   DP-T @ ;
: ALLOT-T ( n -- )       HERE-T THERE OVER ERASE   DP-T +! ;
: C,-T    ( char -- )   HERE-T C!-T   1 DP-T +! ;
: W,-T    ( w -- )      HERE-T W!-T   2 DP-T +! ;
: ,-T     ( n -- )      HERE-T  !-T   CELL-T DP-T +! ;
: S,-T    ( addr len -- )
   0 ?DO   COUNT C,-T   LOOP   DROP ;

: ALIGN  BEGIN HERE-T CELL-T 1 - AND WHILE 0 C,-T REPEAT ;

: tdump  target-image here-t dump ;

\ Output to prims.inc
: ?ERR  ABORT" file I/O error" ;

VARIABLE OUT
: OPEN   R/W CREATE-FILE ?ERR OUT ! ;
: WRITE  ( adr len -- )  OUT @ WRITE-FILE ?ERR ;

CREATE EOL 1 C, 0A C,
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

: SAVE-IMG
    R/W CREATE-FILE ?ERR
    DUP TARGET-IMAGE HERE-T ROT WRITE-FILE ?ERR
    CLOSE-FILE ?ERR ;

: SAVE-INC
    OPEN  BASE @ DECIMAL
    HERE-T 0 DO
        I THERE 10 0 DO
            COUNT 0 <# #S #> WRITE  S" ," WRITE
        LOOP DROP
        NEWLINE
    10 +LOOP  CLOSE  BASE ! ;

: SAVE  ( -- )
    CLOSE  close-info
    CR ." Saving " BASE @ DECIMAL HERE-T . BASE ! ." bytes..."
    S" kernel.img" SAVE-IMG
    S" kernel.inc" SAVE-INC
    ." done" ;

S" prims.inc" OPEN
S" see.info" open-info

: ciao cr bye ;

\ **********************************************************************
\ Compiler

VARIABLE ?CODE
: LATEST ( -- n )  ?CODE @ DUP IF C@-T ELSE INVERT THEN ;
: PATCH  ( n -- )  ?CODE @ DUP 0= ABORT" patch?" C!-T ;
: OP,  ( opcode -- )  HERE-T ?CODE !  C,-T ;

: COMPILE,  ( addr -- )
    DUP C@-T 5F >  OVER 1+ C@-T 0= AND IF  C@-T OP, EXIT  THEN
    1 OP, W,-T ;

: EXIT  0 OP, ;

\ Create Headers in Target Image
VARIABLE LAST
CREATE CONTEXT  1 , 24 , ( FORTH ) 24 , ( COMPILER )
: FORTH     1 CONTEXT ! ; FORTH
: COMPILER  2 CONTEXT ! ;

: EMPLACE  ( targ-h targ-context -- )
    CELL+-T >R  CONTEXT CELL+ 2@  R@ !-T  R> CELL+-T !-T
    HERE-T SWAP !-T ;

: PRUNE  ( store here and context for target )
    HERE-T 8 !-T  CONTEXT CELL+ 2@  1C !-T  20 !-T ;

: HASH   ( voc -- thread )  CELLS CONTEXT + ;

: HEADER   ( -- )
    ALIGN  HERE-T  CONTEXT @ HASH  DUP @ ,-T  !
    HERE-T LAST !  BL WORD COUNT DUP ( 80 OR)  C,-T S,-T  ALIGN ;

VARIABLE STATE-T
: ?EXEC  STATE-T @ 0= ABORT" cannot execute target word!" ;

VARIABLE CSP
: !CSP  DEPTH CSP ! ;
: ?CSP  DEPTH CSP @ - ABORT" definition not finished" ;

: TARGET-CREATE   ( -- )
   >IN @ HEADER >IN !  CREATE  HERE-T ,
   DOES>  ?EXEC  @ COMPILE, ;

: H. . ;
: '-T  ' >BODY @ ;
: HAS ( a -- )  '-T  SWAP !-T ;

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
    CREATE ,  DOES> ?EXEC @ OP, ;
\    CREATE ,  DOES> @  LATEST <LIT> =
\        IF  10 + PATCH  ELSE  OP,  THEN ;

\ Target Literals
: LITERAL  ( n -- )  ?EXEC  8 OP,  ,-T ;
: $   BL WORD NUMBER DROP LITERAL ;

\ Define Meta Branching Constructs
: ?CONDITION  INVERT ABORT" unbalanced" ;
: MARK      ( -- here )     ?EXEC  HERE-T  0 ?CODE ! ;
: >MARK     ( -- f addr )   TRUE  MARK   0 C,-T ;
: >RESOLVE  ( f addr -- )   MARK  OVER -  SWAP C!-T   ?CONDITION ;
: <MARK     ( -- f addr )   TRUE  MARK ;
: <RESOLVE  ( f addr -- )   MARK  - C,-T   ?CONDITION ;

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
: C"  HERE-T  [CHAR] " PARSE S,-T  0 C,-T ; \ c-style string
: ,"  ?EXEC  [CHAR] " PARSE  DUP C,-T  S,-T  0 ?CODE ! ;

: S"      0A OP,  ," ;
: ."      0B OP,  ," ;
: ABORT"  0C OP,  ," ;

: ,    ,-T ;
: C,   C,-T ;
: \\ ;
: { ;
: } ;
: forget ;

\ Defining Words
: CONSTANT  TARGET-CREATE  10 OP, ALIGN   ,-T ;
: VARIABLE  TARGET-CREATE  11 OP, ALIGN 0 ,-T ;

: BUFFER ( n <name> -- )  ALIGN  HERE-T  SWAP ALLOT-T  CONSTANT ;
: TAG  HERE-T  TAG DUP C,-T S,-T  CONSTANT ;

: [   0 STATE-T ! ;
: ]  -1 STATE-T ! ;

: T:  HEADER   0 ?CODE !  ] ;  \ to create words with no host header

: ;_  [COMPILE] ; ; IMMEDIATE \ concession
: ;   ?CSP EXIT [ ;
: :   TARGET-CREATE  0 ?CODE !  !CSP ] ;_

