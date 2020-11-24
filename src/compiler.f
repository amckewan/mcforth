( Optimizing compiler )

FORTH HEX

: LATEST ( -- op/0 )    ?CODE @ DUP IF C@ THEN ;
: PATCH  ( op -- )      ?CODE @ C! ;
: REMOVE ( -- )         0 ?CODE 2@  H !  ?CODE 2! ;

: LIT?  ( -- f )  ?CODE @ DUP IF  C@ 20 =  THEN ;
: LIT@  ( -- n )  ?CODE @ 1 + @ ;
: LIT!  ( n -- )  ?CODE @ 1 + ! ;

: BINARY  ( op -- )
    CREATE C,  DOES> C@
    LIT? IF  LIT@ REMOVE
        LIT? IF  LIT@ SWAP ( op n1 n2 )
            ROT HERE !  HERE EXECUTE  LIT!
        ELSE
            SWAP 40 XOR OP, ,
        THEN
    ELSE  OP,
    THEN ;

: UNARY ( n binary-xt forth-xt -- )
    CREATE , SWAP , ,  DOES> ( fxt n bxt )
    LIT? IF    LIT@  SWAP 2@ EXECUTE  LIT!
         ELSE  CELL+ 2@ \\ LITERAL EXECUTE  THEN ;

COMPILER
61 BINARY +     62 BINARY -     63 BINARY *     64 BINARY /
65 BINARY AND   66 BINARY OR    67 BINARY XOR

   1 ' + FORTH ' + COMPILER UNARY 1+
   1 ' - FORTH ' - COMPILER UNARY 1-
CELL ' + FORTH ' + COMPILER UNARY CELL+
CELL ' * FORTH ' * COMPILER UNARY CELLS

\ 70-72 and 78-7A not used for lit+op
73 BINARY =     74 BINARY <     75 BINARY >     76 BINARY U<      77 BINARY U>

\ do we want any of these? or just use NOT?
7B BINARY <>    \ 7C BINARY >=    7D BINARY <=   \ 7E BINARY U>=     7F BINARY U<=

: NOT  ( invert last conditional op )
    LATEST  DUP 70 80 WITHIN  OVER F7 AND 33 38 WITHIN OR
    IF  8 XOR PATCH  ELSE  DROP 70 OP, ( 0= )  THEN ;
FORTH

: MEMORY  ( op -- )
    CREATE C,  DOES> C@
      LIT? IF  40 XOR PATCH  ELSE  OP,  THEN ;
COMPILER
68 MEMORY @     69 MEMORY !     6A MEMORY +!
FORTH

: CONDITION ( -- )  LATEST
    DUP        70 80 WITHIN IF ( cond )      20 - PATCH  ELSE
    DUP F7 AND 33 38 WITHIN IF ( lit-cond )  10 + PATCH  ELSE
    DROP 58 OP, ( 0<>IF ) THEN THEN ;

COMPILER
: IF        CONDITION  >MARK ;
: UNTIL     CONDITION  <RESOLVE ;
: WHILE     \\ IF  SWAP ;
FORTH

DECIMAL
