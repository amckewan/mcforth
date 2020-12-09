( the rest of forth )

DECIMAL

: S,  ( a n -- )  BEGIN DUP WHILE >R COUNT C, R> 1- REPEAT 2DROP ;
: ,"        '"' PARSE  DUP C, S,  -OPT ;
COMPILER
: S"        $A C, ," ;
: ."        $B C, ," ;
: ABORT"    $C C, ," ;
FORTH

-1 CONSTANT TRUE
 0 CONSTANT FALSE

: ON        TRUE  SWAP ! ;
: OFF       FALSE SWAP ! ;

: TUCK      SWAP OVER ;
: -ROT      ROT ROT ;

: 2>R       SWAP >R >R ;
: 2R>       R> R> SWAP ;
: 2R@       R> R> 2DUP >R >R SWAP ;

: NOT  0= ;
: 0<>  0= NOT ;
: <>   = NOT ;

: CHAR  BL WORD 1+ C@ ;
COMPILER
: [CHAR]  CHAR \\ LITERAL ;
: [']     ' dA @ - \\ LITERAL ;
FORTH

: BLANK  BL FILL ;
: ERASE  0 FILL ;
: PLACE  ( a n a' -- )  2DUP C!  1+ SWAP MOVE ;

: /STRING  ROT OVER +  -ROT - ;
: -TRAILING  ( a n -- a n' )
    BEGIN  DUP WHILE  2DUP + 1- C@ BL = WHILE  1-  REPEAT THEN ;

: ABS       DUP 0< IF NEGATE THEN ;
: MIN       2DUP > IF SWAP THEN DROP ;
: MAX       2DUP < IF SWAP THEN DROP ;
: S>D       DUP 0< ;

: */MOD     >R M* R> SM/REM ;
: */        */MOD NIP ;
: /MOD      >R S>D R> SM/REM ;

: SPACES    0 MAX  0 ?DO  SPACE  LOOP ;

: \S        BEGIN REFILL 0= UNTIL ;

: :NONAME   ALIGN HERE ] ;

COMPILER
: POSTPONE  2 -' IF  1 -FIND ABORT" ?" \\ LITERAL
    [ FORTH ' COMPILE, COMPILER ] LITERAL  THEN  COMPILE, ;
FORTH

: EVALUATE ( a n -- )  0 >SOURCE  INTERPRET  SOURCE> ;

: MARKER  ALIGN HERE  CONTEXT CELL+ 2@ , ,  CREATE ,
    DOES> @  DUP H !  2@ CONTEXT CELL+ 2!  FORTH ;

: VALUE  HEADER  $13 , , ;
: TO  ' >BODY ! ;
COMPILER
: TO  ' >BODY \\ LITERAL POSTPONE ! ;
FORTH

: DEFER  HEADER  $14 , CELL , ( abort ) ;
: IS  ' >BODY ! ;
COMPILER
: IS  \\ TO ;
FORTH

\ Interpreter string literals
CREATE SBUF 300 ALLOT
VARIABLE #SBUF
: STASH ( a n -- a' n )  DUP 300 U> ABORT" too big for stash"
    DUP #SBUF @ + 300 > IF  #SBUF OFF ( wrap ) THEN
    #SBUF @  OVER #SBUF +!  SBUF + SWAP  ( a a' n )
    DUP >R OVER >R  MOVE  R> R> ;
: S"  [CHAR] " PARSE STASH ;

\ Pictured numeric output
\ Adapted from Wil Baden's ThisForth
VARIABLE HLD
: PAD       HERE 200 + ;
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

\ Save dictionary image
: ?IOERR  ABORT" File I/O Error" ;
: SAVE ( <filename> -- ) \ format for include
    PARSE-NAME W/O CREATE-FILE ?IOERR
    HERE 0 DO
        DUP I C@ 0 <# ',' HOLD #S #> ROT
        I 15 AND 15 = IF WRITE-LINE ELSE WRITE-FILE THEN ?IOERR
    LOOP
    CLOSE-FILE ?IOERR ;
: SAVE-IMAGE ( <filename> -- )
    PARSE-NAME W/O CREATE-FILE ?IOERR
    DUP 0 HERE ROT WRITE-FILE ?IOERR
    CLOSE-FILE ?IOERR ;
