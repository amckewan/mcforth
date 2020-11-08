: \         SOURCE >IN ! DROP ; IMMEDIATE
: (         ')' PARSE 2DROP ; IMMEDIATE
: .(        ')' PARSE TYPE  ; IMMEDIATE

( ***** Preamble ***** )

5 5 + BASE !
: DECIMAL   10 BASE ! ;
: HEX       16 BASE ! ;  HEX

: FORTH     1 CONTEXT ! ;
: COMPILER  2 CONTEXT ! ;

: <MARK     HERE  0 ?CODE ! ;
: <RESOLVE  <MARK  - C, ;
: >MARK     <MARK  0 C, ;
: >RESOLVE  <MARK  OVER -  SWAP C! ;

COMPILER
: IF        $58 C, >MARK ;
: THEN      >RESOLVE ;
: ELSE      3 C, >MARK  SWAP >RESOLVE ;

: BEGIN     <MARK ;
: AGAIN       3 C, <RESOLVE ;
: UNTIL     $58 C, <RESOLVE ;
: WHILE     \\ IF  SWAP ;
: REPEAT    \\ AGAIN  \\ THEN ;

: DO        4 C,  >MARK  <MARK ;
: ?DO       5 C,  >MARK  <MARK ;
: LOOP      6 C,  <RESOLVE  >RESOLVE ;
: +LOOP     7 C,  <RESOLVE  >RESOLVE ;
FORTH
