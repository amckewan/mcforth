
: IMMEDIATE  PREVIOUS  $40 OR   SWAP C! ;

: \         SOURCE >IN ! DROP ; IMMEDIATE
: (         ')' PARSE 2DROP ; IMMEDIATE

: [COMPILE] ' COMPILE, ; IMMEDIATE

( This is enough to load the compiler, yet keep it optional. )

5 5 + BASE !
: DECIMAL   10 BASE ! ;
: HEX       16 BASE ! ;

: <MARK     HERE  -OPT ;
: <RESOLVE  <MARK  - C, ;
: >MARK     <MARK  0 C, ;
: >RESOLVE  <MARK  OVER -  SWAP C! ;

: IF        $58 C, >MARK              ; IMMEDIATE
: THEN      >RESOLVE                  ; IMMEDIATE
: ELSE      3 C, >MARK  SWAP >RESOLVE ; IMMEDIATE

: BEGIN     <MARK             ; IMMEDIATE
: AGAIN       3 C, <RESOLVE   ; IMMEDIATE
: UNTIL     $58 C, <RESOLVE   ; IMMEDIATE
: WHILE     [COMPILE] IF  SWAP       ; IMMEDIATE
: REPEAT    [COMPILE] AGAIN  [COMPILE] THEN ; IMMEDIATE

: ?DO       4 C,  >MARK  <MARK       ; IMMEDIATE
: DO        5 C,  >MARK  <MARK       ; IMMEDIATE
: LOOP      6 C,  <RESOLVE  >RESOLVE ; IMMEDIATE
: +LOOP     7 C,  <RESOLVE  >RESOLVE ; IMMEDIATE
