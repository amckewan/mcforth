compiler
: \  source >in ! drop ;
: (  41 parse 2drop ;
: .( 41 parse type ;
forth
: \  [COMPILE] \ ;
: (  [COMPILE] ( ;
: .( [COMPILE] .( ;

\ Standard core words (most of them)

-1 CONSTANT TRUE
 0 CONSTANT FALSE

 \ not standard but convenient
 : ON   TRUE  SWAP ! ;
 : OFF  FALSE SWAP ! ;

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

: DECIMAL  10 BASE ! ;
: HEX  16 BASE ! ;

: 1+  1 + ;
: 1-  1 - ;

: 0<>  0= 0= ;
: 0<  0 < ;
: 0>  0 > ;
: <>  = 0= ;
: NOT  0= ;

: CELL+  1 CELLS + ;
: COUNT  DUP 1 +  SWAP C@ ;

: CHAR  BL WORD 1+ C@ ;
COMPILER
: [CHAR]  CHAR [COMPILE] LITERAL ;
: [']     ' [COMPILE] LITERAL ;
FORTH
: CHAR+  1 + ;
: CHARS  ;

: PLACE  ( a n a' -- )  2DUP C!  1+ SWAP MOVE ;

\ : >BODY  2 CELLS + ;
\ : DOES>  COMPILE (DOES>)  [COMPILE] EXIT ; IMMEDIATE

VARIABLE ?CODE
: OP,  HERE ?CODE ! C, ;

( *** Branching and Looping *** )
: ?CONDITION  INVERT ABORT" unbalanced" ;
: MARK  ( -- here )  HERE  0 ?CODE ! ;
: ?>MARK      ( -- f addr )   TRUE  MARK   0 , ;
: ?>RESOLVE   ( f addr -- )   MARK  OVER -  SWAP !   ?CONDITION ;
: ?<MARK      ( -- f addr )   TRUE  MARK ;
: ?<RESOLVE   ( f addr -- )   MARK  - ,   ?CONDITION ;

\ HEX ( does nothing)
: CONDITION  ( optimizer todo )
    88 OP, ;

\ : NOT  ( invert last conditional op )  LATEST 40 48 WITHIN
\     IF  LATEST 8 + PATCH  ELSE  40 OP,  THEN ; IMMEDIATE
: NOT 0= ;
\ DECIMAL

COMPILER
: IF        CONDITION  ?>MARK ;
: THEN      ?>RESOLVE ;
: ELSE      2 OP,  ?>MARK  2SWAP ?>RESOLVE ;
: BEGIN     ?<MARK ;
: UNTIL     CONDITION  ?<RESOLVE ;
: AGAIN     2 OP,  ?<RESOLVE ;
: WHILE     [COMPILE] IF  2SWAP ;
: REPEAT    [COMPILE] AGAIN  [COMPILE] THEN ;
FORTH

\ : DO     COMPILE (DO)     >MARK <MARK ; IMMEDIATE
\ : ?DO    COMPILE (?DO)    >MARK <MARK ; IMMEDIATE
\ : LOOP   COMPILE (LOOP)   <RESOLVE >RESOLVE ; IMMEDIATE
\ : +LOOP  COMPILE (+LOOP)  <RESOLVE >RESOLVE ; IMMEDIATE

( *** xxx *** )
: ?DUP      DUP IF DUP THEN ;
: ABS       DUP 0< IF NEGATE THEN ;
: MIN       2DUP > IF SWAP THEN DROP ;
: MAX       2DUP < IF SWAP THEN DROP ;
: S>D       DUP 0< ;

: */MOD     >R M* R> SM/REM ;
: */        */MOD NIP ;
: /MOD      >R S>D R> SM/REM ;

abort
\ : SPACES    0 ?DO  SPACE  LOOP ;
: SPACES    BEGIN DUP WHILE SPACE 1- REPEAT DROP ;

\ : POSTPONE  (') 0< IF  COMPILE COMPILE ,  ELSE  COMPILE,  THEN ; IMMEDIATE

( *** String literals *** )
CREATE SBUF 200 ALLOT
VARIABLE #SBUF
: STASH ( a n -- a' n )  200 MIN
    DUP #SBUF @ + 200 > IF  #SBUF OFF ( wrap ) THEN
    #SBUF @  OVER #SBUF +!  SBUF + SWAP  ( a a' n )
    DUP >R OVER >R  MOVE  R> R> ;
: S"  [CHAR] " PARSE STASH ;


( *** Pictured numeric output *** )
\ Adapted from Wil Baden's ThisForth
VARIABLE HLD
: PAD       HERE 80 + ;
: >char     dup 10 < not if 10 - [char] A + [char] 0 - then [char] 0 + ;
: <#        PAD HLD ! ;
: HOLD      HLD @ 1 -  DUP HLD !  C! ;
: HOLDS     BEGIN DUP WHILE 1- 2DUP + C@ HOLD REPEAT 2DROP ;
: SIGN      0< IF [CHAR] - HOLD THEN ;
: #         0 BASE @ UM/MOD >R BASE @ UM/MOD SWAP >char HOLD R> ;
: #S        BEGIN   #   2DUP OR 0 = UNTIL ;
: #>        2DROP  HLD @  PAD OVER - ;
: (.)       dup >r  abs  0 <# #s r> sign #> ;
: .         (.) TYPE   SPACE ;
: .R        >R (.) R> OVER - SPACES TYPE ;
: U.        0 <# #S #> TYPE   SPACE ;
: U.R       >R 0 <# #S #> R> OVER - SPACES  TYPE ;
: ?         @ . ;
