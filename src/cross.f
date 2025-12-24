\ Cross compiler

warnings off

: TAG S" cross" ;

HEX

CREATE EOL 1 C, 0A C,
: H, , ;

8 CONSTANT CELL

\ Memory Access Words
10000 CONSTANT ORIGIN ( start of dictionary on target, first 64K not used )
CREATE IMAGE 2000 ALLOT   IMAGE 2000 ERASE
: THERE  ( taddr -- addr )   ORIGIN -  IMAGE + ;
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

VARIABLE H  ORIGIN H !
: HERE  ( -- taddr )   H @ ;
: ALLOT ( n -- )       H +! ;
: C,    ( char -- )    HERE TC!      1 H +! ;
: ,     ( n -- )       HERE  T!   CELL H +! ;
: S,    ( addr len -- )
   0 ?DO   COUNT C,   LOOP   DROP ;

: ALIGN  BEGIN HERE CELL 1 - AND WHILE 0 C, REPEAT ;

: TDUMP  SWAP THERE SWAP DUMP ;

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
    0 <# # # '$' hold #> seer @ write-file ?err
    s"  OP " seer @ write-file ?err
    >in @  bl word count seer @ write-line ?err  >in ! ;

: CLOSE  OUT @ CLOSE-FILE ?ERR ;

\ save image
: SAVE-IMG
    R/W CREATE-FILE ?ERR
    DUP IMAGE HERE ORIGIN - ROT WRITE-FILE ?ERR
    CLOSE-FILE ?ERR ;

: SAVE-INC
    OPEN  BASE @ DECIMAL
    HERE ORIGIN DO
        I THERE 10 0 DO
            COUNT 0 <# #S #> WRITE  S" ," WRITE
        LOOP DROP
        NEWLINE
    10 +LOOP  CLOSE  BASE ! ;

: SAVE  ( -- )
    CLOSE  close-info
    CR ." Saving " BASE @ DECIMAL HERE ORIGIN - . BASE ! ." bytes..."
    S" kernel.img" SAVE-IMG
    S" kernel.inc" SAVE-INC
    ." done" ;

S" prims.inc" OPEN
S" see.info" open-info

: ciao cr bye ;

\ **********************************************************************
\ Compiler

: +ORIGIN  ( n -- ta )  CELL * ORIGIN + ;

: COMPILE,  ( addr -- )
    DUP TC@ 5F >  OVER 1+ TC@ 0= AND IF  TC@ C, EXIT  THEN
    DUP CELL 1- AND ABORT" xt not aligned"
    1 C, CELL / DUP C, 8 RSHIFT C, ( le) ;

: EXIT  0 C, ;

\ Create Headers in Target Image
VARIABLE LAST
: DONE  ( store here and context for target )
    HERE 2 +ORIGIN T!  LAST @ 6 +ORIGIN T! ;

: HEADER   ( -- )
    ALIGN  HERE  LAST  DUP @ ,  !
    BL WORD COUNT  DUP C, S,  ALIGN ;

: PRIOR ( -- nfa count )  LAST @ CELL +  DUP TC@ ;

VARIABLE STATE-T
: ?EXEC  STATE-T @ 0= ABORT" cannot execute target word!" ;

VARIABLE CSP
: !CSP  DEPTH CSP ! ;
: ?CSP  DEPTH CSP @ - ABORT" definition not finished" ;

: TARGET-CREATE   ( -- )
   >IN @ HEADER >IN !  CREATE  HERE H,
   DOES>  ?EXEC  @ COMPILE, ;

: H.  . ;
: T'  ' >BODY @ ;
: HAS ( n -- )  T' SWAP +ORIGIN T! ;

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
: ---  1 OP +! ;

: CODE   >IN @ TARGET-CREATE >IN !  OP @ C,  EXIT  OP: ;

\ Target Literals
: LITERAL  ( n -- )  ?EXEC  20 C,  , ;
: $   BL WORD NUMBER DROP LITERAL ;
: [']  T' LITERAL ;

\ Target branching constructs
: ?CONDITION  INVERT ABORT" unbalanced" ;
: MARK      ( -- here )     ?EXEC  HERE  ;
: >MARK     ( -- f addr )   TRUE  MARK   0 C, ;
: >RESOLVE  ( f addr -- )   MARK  OVER -  SWAP TC!   ?CONDITION ;
: <MARK     ( -- f addr )   TRUE  MARK ;
: <RESOLVE  ( f addr -- )   MARK  - C,   ?CONDITION ;

: NOT  ?EXEC  70 C, ;

: IF        58 C,  >MARK ;
: THEN      >RESOLVE ;
: ELSE      3 C,  >MARK  2SWAP >RESOLVE ;
: BEGIN     <MARK ;
: UNTIL     58 C,  <RESOLVE ;
: AGAIN     3 C,  <RESOLVE ;
: WHILE     IF  2SWAP ;
: REPEAT    AGAIN  THEN ;

: ?DO       4 C,  >MARK  <MARK ;
: DO        5 C,  >MARK  <MARK ;
: LOOP      6 C,  <RESOLVE  >RESOLVE ;
: +LOOP     7 C,  <RESOLVE  >RESOLVE ;

\ Compile Strings into the Target
: C"   HERE  [CHAR] " PARSE S, 0 C, ; \ c-style string
: ,"         [CHAR] " PARSE DUP C, S, ;

: S"      A C,  ," ;
: ."      B C,  ," ;
: ABORT"  C C,  ," ;

: { ;
: } ;
: forget ;
: ,A  , ;
: [COMPILE] ;
: 0, 0 , ;

\ : CELL+ CELL + ;
\ : CELLS CELL * ;

\ Defining Words
: CONSTANT  TARGET-CREATE  10 C, ALIGN   , ;
: VARIABLE  TARGET-CREATE  11 C, ALIGN 0 , ;

: BUFFER ( n <name> -- )  ALIGN  HERE  SWAP ALLOT  CONSTANT ;
: TAG  HERE  TAG DUP C, S,  CONSTANT ;

: [   0 STATE-T ! ;
: ]  -1 STATE-T ! ;

: T:  HEADER  ] ;  \ to create words with no host header

: ;_  POSTPONE ; ; IMMEDIATE
: IMMEDIATE  PRIOR 40 OR SWAP TC! ;
: ;   ?CSP EXIT [ ;
: :   TARGET-CREATE !CSP ] ;_

include ./kernel.f

DONE
SAVE
