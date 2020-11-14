\ include std/core
\ some of standard programming tools
\ require core-ext
\ require std-tools

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
    REFILL 0= UNTIL  DROP ; IMMEDIATE

: [IF]  0= IF  POSTPONE [ELSE]  THEN ; IMMEDIATE
: [THEN] ; IMMEDIATE

: HAVE ( -- f ) BL WORD FIND NIP ;
: [DEFINED]   HAVE ; IMMEDIATE
: [UNDEFINED] HAVE NOT ; IMMEDIATE
