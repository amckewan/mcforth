: FORTH     1 CONTEXT ! ;
: COMPILER  2 CONTEXT ! ;

COMPILER
: \         SOURCE >IN ! DROP ;
: (         ')' PARSE 2DROP ;
FORTH
: \         \\ \ ;
: (         \\ ( ;

( This is enough to load the compiler, yet keep it optional. )

5 5 + BASE !
: DECIMAL   10 BASE ! ;
: HEX       16 BASE ! ;

: <MARK     HERE  -OPT ;
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
