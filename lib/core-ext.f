( core extension words )

 0 CONSTANT FALSE
-1 CONSTANT TRUE

NEED 2>R : 2>R       SWAP >R >R ;
NEED 2R> : 2R>       R> R> SWAP ;
NEED 2R@ : 2R@       R> R> 2DUP >R >R SWAP ;

: .(        ')' PARSE TYPE ; IMMEDIATE

\ have to remember current, context as well
\ as here to trim all wordlists
: MARKER  ALIGN HERE  FORTH-WORDLIST @ ,  CREATE ,
    DOES> @  DUP H !  @ FORTH-WORDLIST ! ;

: VALUE  HEADER  $13 , , ;
: (TO) ( xt -- )  >BODY  STATE @ IF POSTPONE LITERAL POSTPONE ! ELSE ! THEN ;
: TO  ' (TO) ; IMMEDIATE

: DEFER  HEADER  $14 , ['] ABORT , ;
: IS  POSTPONE TO ; IMMEDIATE

: C"  '"' PARSE  POSTPONE SLITERAL  POSTPONE DROP  POSTPONE 1- ; IMMEDIATE

: ERASE  0 FILL ;

: TUCK      SWAP OVER ;

: CASE      0 ; IMMEDIATE
: OF        $D C, >MARK ; IMMEDIATE
: ENDOF     POSTPONE ELSE ; IMMEDIATE
: ENDOF;    POSTPONE EXIT  POSTPONE THEN ; IMMEDIATE
: ENDCASE   POSTPONE DROP  BEGIN ?DUP WHILE >RESOLVE REPEAT ; IMMEDIATE

\ Use SBUF and HLD for S\"
: +CHAR   HLD @ C!  1 HLD +! ;

0 [IF] \ This fails since it exceeds the 8-bit branch reach
: \CHAR  ( char -- n )
    CASE
        'a' OF  7 ENDOF
        'b' OF  8 ENDOF
        'e' OF 27 ENDOF
        'f' OF 12 ENDOF
        'l' OF 10 ENDOF
        'm' OF 13 +CHAR 10 ENDOF
        'n' OF 10 ENDOF
        'q' OF 34 ENDOF
        'r' OF 13 ENDOF
        't' OF  9 ENDOF
        'v' OF 11 ENDOF
        'x' OF COUNT DIGIT 4 LSHIFT >R COUNT DIGIT R> OR ENDOF
        'z' OF  0 ENDOF
        \ '"' OF 34 ENDOF
        \ '\' OF 92 ENDOF
        DUP
    ENDCASE ;
[ELSE]
: \CHAR  ( char -- n )
    ( could put the most common ones first )
    'a' OF  7 ENDOF;
    'b' OF  8 ENDOF;
    'e' OF 27 ENDOF;
    'f' OF 12 ENDOF;
    'l' OF 10 ENDOF;
    'm' OF 13 +CHAR 10 ENDOF;
    'n' OF 10 ENDOF;
    'q' OF 34 ENDOF;
    'r' OF 13 ENDOF;
    't' OF  9 ENDOF;
    'v' OF 11 ENDOF;
    'x' OF COUNT DIGIT 4 LSHIFT >R COUNT DIGIT R> OR ENDOF;
    'z' OF  0 ENDOF;
    \ '"' OF 34 ENDOF;
    \ '\' OF 92 ENDOF;
    ;
[THEN]

: PARSE\"  ( -- addr len )
    'SBUF DUP HLD !
    SOURCE OVER +  OVER >IN @ + ( src end src' )
    BEGIN  2DUP U> NOT ABORT" oops"
           COUNT  DUP '"' = NOT
    WHILE  '\' OF  COUNT \CHAR  THEN  +CHAR
    REPEAT DROP
    NIP SWAP - >IN !
    HLD @ OVER - ;

: S\"  PARSE\"  STATE @ IF [COMPILE] SLITERAL THEN ; IMMEDIATE
