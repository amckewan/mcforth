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

: 2OVER     3 PICK 3 PICK ;
: 2SWAP     ROT >R ROT R> ;
\ only with multi-op inlining
\ or just add to compiler
: 2>R       SWAP >R >R ;
: 2R>       R> R> SWAP ;
: 2R@       R> R> 2DUP >R >R SWAP ;

: ENVIRONMENT?  2DROP FALSE ;

: NOT  0= ;
: 0<>  0= NOT ;
: <>   = NOT ;

: CHAR  BL WORD 1+ C@ ;
COMPILER
: [CHAR]  CHAR \\ LITERAL ;
: [']     ' \\ LITERAL ;
FORTH

: BLANK  BL FILL ;
: ERASE  0 FILL ;
: PLACE  ( a n a' -- )  2DUP C!  1+ SWAP MOVE ;

: S=  COMPARE 0= ;
: /STRING  ROT OVER +  ROT ROT - ;
: -TRAILING  ( a n -- a n' )
    BEGIN  DUP WHILE  2DUP + 1- C@ BL = WHILE  1-  REPEAT THEN ;

\ Multi-line comments, using (( ... )) so we can use () inside
: ((    BEGIN   SOURCE  >IN @ /STRING  S" ))" SEARCH NOT
        WHILE   2DROP  REFILL 0= ABORT" Missing ))"
        REPEAT  SOURCE ROT - 2 + >IN !  2DROP ;

( *** more stuff *** )
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
: S"  [CHAR] " PARSE STASH ;

( *** Pictured numeric output *** )

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

: EVALUATE ( a n -- )
    -1 >SOURCE  >IN CELL+ 2!  >IN OFF  INTERPRET  SOURCE> ;

: MARKER  ALIGN HERE  CONTEXT CELL+ 2@ , ,  CREATE ,
    DOES> @  DUP H !  2@ CONTEXT CELL+ 2!  FORTH ;

( *** Value *** )

: VALUE  HEADER  $13 , , ;
: TO  ' >BODY ! ;
COMPILER
: TO  ' >BODY \\ LITERAL POSTPONE ! ;
FORTH

( *** Defer *** )

: DEFER  HEADER  $14 , CELL , ( abort ) ;
: IS  ' >BODY ! ;
COMPILER
: IS  \\ TO ;
FORTH
