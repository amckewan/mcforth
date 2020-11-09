( core words on top of kernel )

DECIMAL

: ,"        '"' PARSE  DUP C, S,  -OPT ;
COMPILER
: "         $A C, ," ;
: S"        $A C, ," ;
: ."        $B C, ," ;
: ABORT"    $C C, ," ;
FORTH

-1 CONSTANT TRUE
 0 CONSTANT FALSE

: ON        TRUE  SWAP ! ;
: OFF       FALSE SWAP ! ;

: NIP       SWAP DROP ;
: TUCK      SWAP OVER ;
: -ROT      ROT ROT ;

: 2DUP      OVER OVER ;
: 2DROP     DROP DROP ;
: 2OVER     3 PICK 3 PICK ;
: 2SWAP     ROT >R ROT R> ;
: 2>R       SWAP >R >R ;
: 2R>       R> R> SWAP ;

: ENVIRONMENT?  2DROP FALSE ;
\ : ABORT  -1 THROW ;
\ : QUIT  -56 THROW ;

32 CONSTANT BL
: SPACE  BL EMIT ;
: CR  10 EMIT ;

: 1+  1 + ;
: 1-  1 - ;

: NOT  0= ;
: 0<>  0= NOT ;
: <>   = NOT ;

: CELL+  1 CELLS + ;
: COUNT  DUP 1 +  SWAP C@ ;

: CHAR  BL WORD 1+ C@ ;
COMPILER
: [CHAR]  CHAR \\ LITERAL ;
: [']     ' \\ LITERAL ;
FORTH

: ERASE  0 FILL ;
: PLACE  ( a n a' -- )  2DUP C!  1+ SWAP MOVE ;

\ multi-line comments
: (   BEGIN  [CHAR] ) PARSE 2DROP
        SOURCE-ID 1+ 2 U< IF EXIT THEN
        SOURCE DROP  >IN @ +  1- C@ [CHAR] ) = IF EXIT THEN
        REFILL 0= ABORT" Missing )"
      AGAIN ; IMMEDIATE

( *** more stuff *** )
: ?DUP      DUP IF DUP THEN ;
: ABS       DUP 0< IF NEGATE THEN ;
: MIN       2DUP > IF SWAP THEN DROP ;
: MAX       2DUP < IF SWAP THEN DROP ;
: S>D       DUP 0< ;

: */MOD     >R M* R> SM/REM ;
: */        */MOD NIP ;
: /MOD      >R S>D R> SM/REM ;

: SPACES    0 MAX  0 ?DO  SPACE  LOOP ;

: \S        BEGIN REFILL 0= UNTIL ;


( *** Interpreter string literals *** )

CREATE SBUF 300 ALLOT
VARIABLE #SBUF
: STASH ( a n -- a' n )  DUP 300 U> ABORT" too big for stash"
    DUP #SBUF @ + 300 > IF  #SBUF OFF ( wrap ) THEN
    #SBUF @  OVER #SBUF +!  SBUF + SWAP  ( a a' n )
    DUP >R OVER >R  MOVE  R> R> ;
:  "  [CHAR] " PARSE STASH ;
: S"  [CHAR] " PARSE STASH ;


( *** Pictured numeric output *** )

\ Adapted from Wil Baden's ThisForth
VARIABLE HLD
: PAD       HERE 80 + ;
: <#        PAD HLD ! ;
: HOLD      HLD @ 1 -  DUP HLD !  C! ;
: HOLDS     BEGIN DUP WHILE 1- 2DUP + C@ HOLD REPEAT 2DROP ;
: SIGN      0< IF [CHAR] - HOLD THEN ;
: >char     dup 10 < not if [ 10 'A' - '0' + ] literal - then '0' + ;
: #         0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP >char HOLD R> ;
: #S        BEGIN   #   2DUP OR 0 = UNTIL ;
: #>        2DROP  HLD @  PAD OVER - ;
: (.)       dup >r  abs  0 <# #s r> sign #> ;
: .         (.) TYPE   SPACE ;
: .R        >R (.) R> OVER - SPACES TYPE ;
: U.        0 <# #S #> TYPE   SPACE ;
: U.R       >R 0 <# #S #> R> OVER - SPACES  TYPE ;
: H.        BASE @ HEX  SWAP U.  BASE ! ;
: ?         @ . ;


( *** Save dictionary image *** )

: ?IOERR  DUP IF CR ." ior = " DUP . THEN ABORT" File I/O Error" ;
: WRITE ( a u fid -- )  WRITE-FILE ?IOERR ;
: .ADDR ( a fid -- )  >R  S" /* " R@ WRITE  (.) R@ WRITE  S"  */ " R> WRITE ;

: SAVE ( <filename> -- ) \ format for include
    BL WORD COUNT W/O CREATE-FILE ?IOERR ( fid)
    HERE 0 DO
        I 15 AND 0= IF  I OVER .ADDR  THEN
        DUP I C@ 0 <# [CHAR] , HOLD #S #> ROT
        I 15 AND 15 = IF WRITE-LINE ELSE WRITE-FILE THEN ?IOERR
    LOOP
    HERE 15 AND IF  DUP 0 0 ROT WRITE-LINE ?IOERR  THEN
    CLOSE-FILE ?IOERR ;

: SAVE-IMAGE ( <filename> -- )
    BL WORD COUNT W/O CREATE-FILE ?IOERR ( fid)
    DUP 0 HERE ROT WRITE
    CLOSE-FILE ?IOERR ;


( *** more stuff *** )

COMPILER
: POSTPONE  2 -' IF  FIND DUP 0= ABORT" ?"
    0< IF  \\ LITERAL  [ FORTH ' COMPILE, COMPILER ] LITERAL  THEN THEN  COMPILE, ;
FORTH

COMPILER
: RECURSE  LAST @ CELL+  COUNT 31 AND +  COMPILE, ;
FORTH
: EVALUATE ( a n -- )
    -1 >SOURCE  >IN CELL+ 2!  >IN OFF  INTERPRET  SOURCE> ;

: s=  compare 0= ;
: /string  rot over +  rot rot - ;

: MARKER  ALIGN HERE  CONTEXT CELL+ 2@ , ,  CREATE ,
    DOES> @  DUP H !  2@ CONTEXT CELL+ 2!  FORTH ;