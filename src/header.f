
[defined] empty [if] empty [then] marker empty decimal

\ New Header Format:
\
\  Name  off len  desciption
\  ----  --- ---  ----------
\  nfa    0   8   name field, count + up to 7 chars, blank filled
\  lfa    8   5   link field, address of previous nfa, 0 to end list
\  flags 13   1   2 = immediate
\  cfa   14   2+  code field, 2-N bytes
\  pfa   16   ?   parameter field for create and variable

: upc ( c -- c' )  dup 'a' - 26 u< if $df and then ;
: upper ( a n -- )  bounds ?do  i c@ upc i c!  loop ;

: bl-word ( -- here )   bl word  dup count  2dup upper  + 6 blank ;
: prepare ( a n -- str )  PAD 8 BLANK  PAD PLACE  PAD  dup count upper ;

VARIABLE LAST   \ nfa of latest word

: HEAD ( str -- )  
    ( name )  @  ALIGN     HERE    ,
    ( link )  LAST @ ,     LAST !
    -2 allot ( cfa ) ;

: CFA ( nfa -- cfa )  14 + ;
: NFA ( cfa -- nfa )  14 - ;

: immediate  2  last @ 13 + c! ;
: smudge     last @  dup c@ $80 xor  swap c! ;

: B  bl-word build ;

: FIND-TARGET3 ( str -- str 0 | cfa 1/-1 )
    DUP @  LAST @  ( str str@ nfa )
    begin dup while  
        2dup @ = if  nip nip  dup cfa  swap 13 + c@ 1-  exit then
        cell+ @  $ffffffffff and
    repeat nip ;

: find4  ( str -- str 0 | cfa 1/-1 )
    last @ find-target3 ;

: f  bl-word find4 ;


: 2BODY 2 + ;

: .NFA ( nfa -- )  COUNT 31 AND  SWAP OVER  7 MIN TYPE
    BEGIN DUP 7 > WHILE '_' EMIT 1- REPEAT DROP  SPACE ;

: TOGGLE ( nfa n -- )  OVER C@ XOR  SWAP C! ;
: SMUDGE     LINK @ $20 TOGGLE ;
: IMMEDIATE  LINK @ $80 TOGGLE ;

: IMMED? ( nfa -- 1/-1 )  -1  SWAP C@ $80 AND IF NEGATE THEN ;

: FINDX ( str -- str 0 | cfa 1/-1 ) \  LINK @ 0= IF 0 EXIT THEN
    DUP @  LINK @
    BEGIN ( str@ lfa ) DUP link <> WHILE  
        2DUP 8 - ( nfa) @  -$C1 AND ( -flags)
        = IF ( found) NIP NIP  DUP 4 + ( cfa)  SWAP 8 - IMMED?  EXIT
        THEN  lfa@ ( next)
    REPEAT 2DROP ( not found ) 0 ;

: FINDX-TARGET ( str -- str 0 | cfa 1/-1 )
    DUP @  LINK @
    BEGIN ( str@ lfa ) DUP WHILE  
        2DUP 8 - ( nfa) @  -$C1 AND ( -flags)
        = IF ( found) NIP NIP  DUP 4 + ( cfa)  SWAP 8 - IMMED?  EXIT
        THEN  H@ ( next)
    REPEAT 2DROP ( not found ) 0 ;

\ links point to name
: FIND-TARGET2 ( str -- str 0 | cfa 1/-1 )
    DUP @  LINK @
    BEGIN ( str@ nfa ) DUP WHILE  
        2DUP @  -$C1 AND ( -flags)
        = IF ( found)  NIP NIP  DUP CFA  SWAP IMMED?  EXIT THEN
        8 + H@ ( next)
    REPEAT 2DROP ( not found ) 0 ;



B FIRST
B SECOND
B THIRD-IS-LONG


0 [if]
int findx(const char *str, cell *cfa) {
    cell finding = *(cell*)str;
    cell link = xxx;
    while (link) {
        cell name = fetch32(link - 8) & -0xC1;
        if (name == finding) {
            *cfa = link + 4;
        }
    }
}
[then]
