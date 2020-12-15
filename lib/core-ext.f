( core extension words )

 0 CONSTANT FALSE
-1 CONSTANT TRUE

: 2>R       SWAP >R >R ;
: 2R>       R> R> SWAP ;
: 2R@       R> R> 2DUP >R >R SWAP ;

( here for completeness but they break the optimizer )
( much better to just say "0= not" in a definition )
: 0<>       0= NOT ;
: <>        = NOT ;

: .(        ')' PARSE TYPE ; IMMEDIATE

\ have to remember current, context as well
\ as here to trim all wordlists
: MARKER  ALIGN HERE  FORTH-WORDLIST @ ,  CREATE ,
    DOES> @  DUP H !  @ FORTH-WORDLIST ! ;

: VALUE  HEADER  $13 , , ;
: (TO) ( xt -- )  >BODY  STATE @ IF POSTPONE LITERAL POSTPONE ! ELSE ! THEN ;
: TO  ' (TO) ; IMMEDIATE

: DEFER  HEADER  $14 , CELL , ( abort ) ;
: IS  POSTPONE TO ; IMMEDIATE

: c"  postpone s" postpone drop postpone 1- ; immediate

: ERASE  0 FILL ;

: TUCK      SWAP OVER ;

: CASE      0 ; IMMEDIATE
: OF        $D C, >MARK ; IMMEDIATE
: ENDOF     POSTPONE ELSE ; IMMEDIATE
: ENDCASE   POSTPONE DROP  BEGIN ?DUP WHILE >RESOLVE REPEAT ; IMMEDIATE


\S

: got ' drop ;
: missing bl word find abort" nope, got it" drop ;

got .r
got 0>

got :noname
got ?do
missing action-of
got again
missing buffer:
\ missing c"
missing case
got compile,
got defer
missing defer!
missing defer@
missing endcase
missing endof
0 CONSTANT FALSE
got hex
got holds
got is
got marker
got nip
missing of
got pad
got parse
got parse-name
got pick
got refill
missing restore-input
missing roll
missing s\"
missing save-input
got source-id
got to
-1 CONSTANT TRUE
got u.r
got u>
got unused
got value
got within
got [compile]
got \
