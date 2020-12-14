( useful extras )

: ON        TRUE  SWAP ! ;
: OFF       FALSE SWAP ! ;

( comment to end of file )
: \S        BEGIN REFILL 0= UNTIL ;

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
          2DUP S" [IF]" COMPARE 0= IF
            2DROP 1+
          ELSE
            2DUP S" [ELSE]" COMPARE 0= IF
              2DROP 1- DUP IF 1+ THEN
            ELSE
              S" [THEN]" COMPARE 0= IF
                1-
              THEN
            THEN
          THEN ?DUP 0= IF EXIT THEN
        REPEAT 2DROP
    REFILL 0= UNTIL  DROP ; IMMEDIATE

: [IF]  0= IF  [COMPILE] [ELSE]  THEN ; IMMEDIATE
: [THEN] ; IMMEDIATE

: DEFINED ( -- f )  BL WORD FIND NIP ;

\ My needs are simpler than require, e.g.
\ need locals from opt/locals.f
\ need off : off 0 swap ! ;
: need  defined if [COMPILE] \ then ;
: from  include ; ( sugar )


\ Save dictionary image
: ?IOERR  ABORT" File I/O Error" ;
: SAVE ( <filename> -- ) \ format for include
    PARSE-NAME W/O CREATE-FILE ?IOERR
    HERE 0 DO
        DUP I C@ 0 <# ',' HOLD #S #> ROT
        I 15 AND 15 = IF WRITE-LINE ELSE WRITE-FILE THEN ?IOERR
    LOOP
    CLOSE-FILE ?IOERR ;
: SAVE-IMAGE ( <filename> -- )
    PARSE-NAME W/O CREATE-FILE ?IOERR
    DUP 0 HERE ROT WRITE-FILE ?IOERR
    CLOSE-FILE ?IOERR ;
