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
