( some standard words )

: CHARS  ; \ useless
: CHAR+  1 + ; \ almost useless
: FM/MOD SM/REM ; \ wrong behavior
: DABS ;

\ F83
\ CODE /MOD   ( num den -- rem quot )
\ BX  POP    AX  POP    CWD    BX CX MOV    DX CX  XOR
\ 0>=  IF    BX  IDIV    2PUSH    THEN
\ BX  IDIV   BX DX  ADD    AX  DEC    2PUSH   END-CODE

: DEFER@  >BODY @ ;
: DEFER!  >BODY ! ;

\ for now...
: IMMEDIATE  PREVIOUS  $40 OR  SWAP C! ;

\ CODE FIND  ( str -- xt flag | str 0 )
\ : POSTPONE  2 -' IF  FIND DUP 0= ABORT" ?"
\    0< IF  \\ LITERAL  [ FORTH ' COMPILE, COMPILER ] LITERAL  THEN THEN  COMPILE, ;

: FIND  2 -FIND IF 1 -FIND NOT ELSE 1 THEN ;
