( core extension words )

: got ' drop ;
: missing bl word find abort" nope, got it" drop ;

: .(   ')' PARSE TYPE ; IMMEDIATE
got .r
: 0<>  0= NOT ;
got 0>

: 2>R       SWAP >R >R ;
: 2R>       R> R> SWAP ;
: 2R@       R> R> 2DUP >R >R SWAP ;

got :noname
: <>        = NOT ;
got ?do
missing action-of
got again
missing buffer:
missing c"
missing case
got compile,
got defer
missing defer!
missing defer@
missing endcase
missing endof
: ERASE  0 FILL ;
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
: TUCK      SWAP OVER ;
got u.r
got u>
missing unused
got value
got within
got [compile]
got \
