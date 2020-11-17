( ***** Optimizing Compiler ***** )

HEX

: LATEST ( -- op/0 )    ?CODE       @ DUP IF C@ THEN ;
: PRIOR  ( -- op/0 )    ?CODE CELL+ @ DUP IF C@ THEN ;
: PATCH  ( op -- )      ?CODE @ C! ;
: UNDO   ( -- )         0 ?CODE 2@  H !  ?CODE 2! ;

: LIT?  ( -- f )  ?CODE @ DUP IF  C@ 8 =  THEN ;
: LIT@  ( -- n )  ?CODE @ 1+ @ ;
: LIT!  ( n -- )  ?CODE @ 1+ ! ;

: UNARY  ( xt -- )
    CREATE ,  DOES> @
    LIT? IF  LIT@ SWAP EXECUTE LIT!  ELSE  COMPILE, THEN ;

\ except for CELLS, not sure how much value these have
' 1+    COMPILER UNARY 1+       FORTH
' 1-    COMPILER UNARY 1-       FORTH
' CELL+ COMPILER UNARY CELL+    FORTH
' CELLS COMPILER UNARY CELLS    FORTH

: BINARY  ( op -- )
    CREATE C,  DOES> C@
    LIT? IF  LIT@ UNDO
        LIT? IF  LIT@ SWAP ( op n1 n2 )
            ROT HERE !  HERE EXECUTE  LIT!
        ELSE
            SWAP 40 XOR OP, ,
        THEN
    ELSE  OP,
    THEN ;

COMPILER
60 BINARY +     61 BINARY -     62 BINARY *     63 BINARY /
64 BINARY AND   65 BINARY OR    66 BINARY XOR

\ 70-72, 78-7A not used for lit+op
73 BINARY =     74 BINARY <     75 BINARY >     76 BINARY U<      77 BINARY U>

\ do we want any of these? or just use NOT
7B BINARY <>    \ 7C BINARY >=    7D BINARY <=   \ 7E BINARY U>=     7F BINARY U<=

: NOT  ( invert last conditional op )
    LATEST  DUP 70 80 WITHIN  OVER F7 AND 33 38 WITHIN OR
    IF  8 XOR PATCH  ELSE  DROP 70 OP, ( 0= )  THEN ;
FORTH

: MEMORY  ( op -- )
    CREATE C,  DOES> C@  LIT?
      IF  40 XOR PATCH  ELSE  OP,  THEN ;
COMPILER
68 MEMORY @     69 MEMORY !     6A MEMORY +!
FORTH

: CONDITION ( -- )  LATEST
    DUP        70 80 WITHIN IF ( cond )      20 - PATCH  ELSE
    DUP F7 AND 33 38 WITHIN IF ( lit-cond )  10 + PATCH  ELSE
    DROP 58 OP, ( 0branch ) THEN THEN ;

COMPILER
: IF        CONDITION  >MARK ;
: UNTIL     CONDITION  <RESOLVE ;
: WHILE     \\ IF  SWAP ;
FORTH

DECIMAL
