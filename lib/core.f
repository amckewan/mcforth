: DECIMAL   #10 BASE ! ; DECIMAL
: HEX       #16 BASE ! ;

: IMMEDIATE  PRIOR  $40 OR  SWAP C! ;

: \         SOURCE >IN ! DROP ; IMMEDIATE
: (         ')' PARSE 2DROP ; IMMEDIATE

: [COMPILE] ' COMPILE, ; IMMEDIATE

: ,"        '"' PARSE  DUP C, S,  -OPT ;
: SLITERAL  $A C, DUP C, S, -OPT ; IMMEDIATE
\ : S"        $A C, ," ; IMMEDIATE
: ."        $B C, ," ; IMMEDIATE
: ABORT"    $C C, ," ; IMMEDIATE

: ABORT     -1 THROW ;

( Conditionals ) HEX
: OFFSET    - DUP 80 + FF U> ABORT" branch too far" ;
: <MARK     HERE  -OPT ;
: <RESOLVE  <MARK  OFFSET C, ;
: >MARK     <MARK  0 C, ;
: >RESOLVE  <MARK  OVER OFFSET  SWAP C! ;

: IF        58 C, >MARK               ; IMMEDIATE
: THEN      >RESOLVE                  ; IMMEDIATE
: ELSE      3 C, >MARK  SWAP >RESOLVE ; IMMEDIATE

: CONDITION ( -- )  LATEST
    DUP        70 80 WITHIN IF ( cond )      20 - PATCH  ELSE
    DUP F7 AND 33 38 WITHIN IF ( lit-cond )  10 + PATCH  ELSE
    DROP 58 OP, ( 0<>IF ) THEN THEN ;

: IF        CONDITION  >MARK ; IMMEDIATE

: BEGIN     <MARK ; IMMEDIATE
: AGAIN     3 C, <RESOLVE ; IMMEDIATE
: UNTIL     CONDITION <RESOLVE ; IMMEDIATE
: WHILE     [COMPILE] IF  SWAP ; IMMEDIATE
: REPEAT    [COMPILE] AGAIN  [COMPILE] THEN ; IMMEDIATE

: ?DO       4 C,  >MARK     <MARK    ; IMMEDIATE
: DO        5 C,  >MARK     <MARK    ; IMMEDIATE
: LOOP      6 C,  <RESOLVE  >RESOLVE ; IMMEDIATE
: +LOOP     7 C,  <RESOLVE  >RESOLVE ; IMMEDIATE
DECIMAL

: NOT       0= ;

: CHAR      BL WORD 1+ C@ ;
: [CHAR]    CHAR [COMPILE] LITERAL ; IMMEDIATE
: [']       ' dA @ - [COMPILE] LITERAL ; IMMEDIATE

: ABS       DUP 0< IF NEGATE THEN ;
: MIN       2DUP > IF SWAP THEN DROP ;
: MAX       2DUP < IF SWAP THEN DROP ;
: S>D       DUP 0< ;

: */MOD     >R M* R> SM/REM ;
: */        */MOD NIP ;
: /MOD      >R S>D R> SM/REM ;

: SPACES    0 MAX  0 ?DO  SPACE  LOOP ;

: POSTPONE  BL WORD FIND  DUP 0= ABORT" ?"
    0< IF  [COMPILE] LITERAL  ['] COMPILE,  THEN  COMPILE, ; IMMEDIATE

: EVALUATE ( a n -- )
    -1 >SOURCE  >IN CELL+ 2!  0 >IN !  HANDLER @
    IF  ['] INTERPRET CATCH SOURCE> THROW  ELSE  INTERPRET SOURCE>  THEN ;

\ Interpreter string literals
\ Standard says minmum 2 * 80 char buffers
CREATE SBUF 2 80 * ALLOT
VARIABLE SBUF#
: 'SBUF ( -- a )  SBUF# @ DUP 1 XOR SBUF# !  80 * SBUF + ;
: STASH ( a n -- a' n )
    DUP>R 80 U> ABORT" too big for stash"
    'SBUF SWAP OVER R@ MOVE R> ;

\ state-smart version
: S"  [CHAR] " PARSE
      STATE @ IF  [COMPILE] SLITERAL  ELSE  STASH  THEN ; IMMEDIATE

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
