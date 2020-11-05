( compiler )
base @ hex

: LATEST ( -- op/-1 )   ?CODE @ DUP IF C@ ELSE INVERT THEN ;
: PATCH  ( op -- )      ?CODE @ DUP 0= ABORT" patch?" C! ;

: BINARY  ( op -- )
    CREATE C,  DOES> C@  LATEST 8 =
      IF  40 XOR PATCH  ELSE  OP,  THEN ;

COMPILER
60 BINARY +     61 BINARY -     62 BINARY *     63 BINARY /
64 BINARY AND   65 BINARY OR    66 BINARY XOR

68 BINARY @     69 BINARY !

\ 70-72, 78-7A not used for lit+op
\ 73 BINARY =  \   74 BINARY <     75 BINARY >     76 BINARY U<      77 BINARY U>

\ do we want any of these? or just use NOT
\ 7B BINARY <>    7C BINARY >=    7D BINARY <=   \ 7E BINARY U>=     7F BINARY U<=

\ 5 < if
: NOT  ( invert last conditional op )
    LATEST  DUP 70 80 WITHIN  OVER F7 AND 33 38 WITHIN OR
    IF  8 XOR PATCH  ELSE  DROP 70 OP, ( 0= )  THEN ;


FORTH
: CONDITION ( -- )  LATEST
    DUP 70 80 WITHIN IF  20 - PATCH  ELSE
    DUP F7 AND 33 38 WITHIN IF  10 + PATCH  ELSE
    DROP 58 OP, THEN THEN ;

COMPILER
: IF        CONDITION  >MARK ;
: UNTIL     CONDITION  <RESOLVE ;
: WHILE     \\ IF  2SWAP ;

FORTH

: 1+ 1 + ;

base !
