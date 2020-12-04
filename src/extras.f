( useful extras )

: .(   ')' PARSE TYPE ; IMMEDIATE

( Multi-line comments )

: COMMENT  CHAR
    BEGIN  DUP DUP PARSE + C@ = NOT
    WHILE  REFILL NOT ABORT" comment?"
    REPEAT DROP ;

comment * this is a test *
comment *
this is also a test*
comment ~
this
is
typical
~
\ comment ~ missing delimiter fails...

( copied from standard )
: [ELSE] ( -- )
    1 BEGIN
        BEGIN PARSE-NAME DUP WHILE
          2DUP S" [IF]" S= IF
            2DROP 1+
          ELSE
            2DUP S" [ELSE]" S= IF
              2DROP 1- DUP IF 1+ THEN
            ELSE
              S" [THEN]" S= IF
                1-
              THEN
            THEN
          THEN ?DUP 0= IF EXIT THEN
        REPEAT 2DROP
    REFILL 0= UNTIL  DROP ;

: [IF]  0= IF  [ELSE]  THEN ;
: [THEN] ;

: DEFINED ( -- f )  1 -' NIP NOT ;

\ My needs are simpler than require, e.g.
\ need locals from opt/locals.f
: need  defined if [COMPILE] \ then ;
: from  include ; ( sugar )
