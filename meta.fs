\ FVM Metacompiler
\ Andrew McKewan
\ December 1994

HEX
\ Support 64-bit gforth host
4 CONSTANT CELL-T
: CELL+-T CELL-T + ;
: CELLS-T CELL-T * ;
: CELL@ UL@ ; \ gforth
: CELL! L! ; \ gforth
\ : CELL, HERE CELL-T ALLOT CELL! ;
warnings off


\ Memory Access Words
CREATE TARGET-IMAGE 2000 ALLOT   TARGET-IMAGE 2000 ERASE
VARIABLE DP-T
: THERE   ( taddr -- addr )   TARGET-IMAGE + ;
: C@-T    ( taddr -- char )   THERE C@ ;
: @-T     ( taddr -- n )      THERE CELL@ ;
: C!-T    ( char taddr -- )   THERE C! ;
: !-T     ( n taddr -- )      THERE CELL! ;
: HERE-T  ( -- taddr )   DP-T @ ;
: ALLOT-T ( n -- )       HERE-T THERE OVER ERASE   DP-T +! ;
: C,-T    ( char -- )   HERE-T C!-T   1 DP-T +! ;
: ,-T     ( n -- )      HERE-T  !-T   CELL-T DP-T +! ;
: S,-T    ( addr len -- )
   0 ?DO   COUNT C,-T   LOOP   DROP ;

: ALIGN  BEGIN HERE-T CELL-T 1 - AND WHILE 0 C,-T REPEAT ;

: tdump  target-image here-t dump ;

\ Output to kernel.c
: ?ERR  ABORT" file I/O error" ;

VARIABLE OUT
: OPEN   R/W CREATE-FILE ?ERR OUT ! ;
: CLOSE  OUT @ CLOSE-FILE ?ERR ;
: WRITE  ( adr len -- )  OUT @ WRITE-FILE ?ERR ;

S" prims.inc" OPEN

CREATE EOL 1 C, 0A C,
: NEWLINE   EOL COUNT WRITE ;
: &  1 PARSE WRITE  NEWLINE ;

: WRITE-DICT-IMG
    S" dict.img" R/W CREATE-FILE ?ERR
    DUP TARGET-IMAGE HERE-T ROT WRITE-FILE ?ERR
    CLOSE-FILE ?ERR ;

: WRITE-DICT \ write dict.c
    S" dict.inc" OPEN
    HERE-T 0 DO
        S" /* " WRITE  I 0 <# # # # # #> WRITE  S"  */ " WRITE  
        I THERE 10 0 DO
            S" 0x" WRITE  COUNT 0 <# # # #> WRITE  S" , " WRITE
        LOOP DROP
        NEWLINE
    10 +LOOP  CLOSE ;

: PRINTABLE  DUP 20 < OVER 7F > OR IF DROP [CHAR] . THEN ;
: WRITE-DICT-DUMP  \ write dump to dict.dump
    S" dict.dump" OPEN
    HERE-T 0 DO
        I 0 <# [CHAR] : HOLD # # # # #> WRITE
        I THERE 10 0 DO
            I 3 AND 0= IF S"  " WRITE THEN
            COUNT 0 <# BL HOLD # # #> WRITE 
        LOOP DROP
        I THERE 10 0 DO  COUNT 0 <# OVER PRINTABLE HOLD #> WRITE  LOOP DROP
        NEWLINE
    10 +LOOP  CLOSE ;

\ Save dictionary image to KERNEL.DCT
: SAVE  ( -- )
    CLOSE
    CR ." Saving..."
    \ WRITE-DICT-IMG
    WRITE-DICT
    WRITE-DICT-DUMP
    ." done" ;

: ciao cr bye ;

\ Opcodes
2F CONSTANT <LIT>

VARIABLE ?CODE
: LATEST  ( -- n )  ?CODE @ C@-T ;
: PATCH  ( n -- )  ?CODE @ C!-T ;
: ,C  ( opcode -- )  HERE-T ?CODE !  C,-T ;
\ : ,A  ( addr -- )   ,C ;

: COMPILE,  ( op/addr -- )
    DUP 200 < IF  ,C  ELSE  FF ,C ,-T  THEN ;

\   ,C ;  ( non-optimizing )
\   DUP 200 < IF  ,C  ELSE  DUP @-T  ( adr op )
\   OVER 2 CELLS + @-T 0=  OVER 20 40 WITHIN AND
\   IF  ( literal ) ,C  CELL+ @-T ,-T  ELSE
\   OVER CELL+ @-T 0=  SWAP 200 < AND
\   IF  ( primative )  @-T ,C  ELSE
\   ( call )  ,A  THEN THEN THEN ;

: EXIT  0 ,C ; IMMEDIATE

\ Create Headers in Target Image
VARIABLE LAST
CREATE CONTEXT  1 , 0 , ( FORTH ) 0 , ( COMPILER )
: FORTH     1 CONTEXT ! ; FORTH
: COMPILER  2 CONTEXT ! ;
: EMPLACE  ( targ-addr -- )  FORTH  \ TODO CONTEXT SWAP THERE 3 CELLS MOVE ;
    CONTEXT @ OVER !-T
    CONTEXT CELL+ @ OVER CELL+-T !-T
    CONTEXT 2 CELLS + @ SWAP 2 CELLS-T + !-T ;

: HASH   ( voc -- thread )  CELLS CONTEXT + ;

: HEADER   ( -- )
    ALIGN  HERE-T  CONTEXT @ HASH  DUP @ ,-T  !
    HERE-T LAST !  BL WORD DUP C@ 1+ S,-T  ALIGN ;

: TARGET-CREATE   ( -- )
   >IN @ HEADER >IN !  CREATE IMMEDIATE  HERE-T ,
   DOES>  @ COMPILE, ;

: RECREATE   ( -- )
   >IN @   TARGET-CREATE   >IN ! ;

\ Generate FVM Primatives
: ?COMMENT  ( allow Forth comment after OP: etc. )
    >IN @  BL WORD COUNT S" (" COMPARE
    IF  >IN !  ELSE  DROP  [COMPILE] (  THEN ;

: C-COMMENT  S" /* " WRITE  >IN @  BL WORD COUNT WRITE  >IN !  S"  */ " WRITE ;
VARIABLE OP  ( next opcode )
: OP!  OP ! ;
: OP:  ( output opcode case statement )
    S" case 0x" WRITE  OP @ 0 <# # # # #> WRITE  S" :  " WRITE
    ?COMMENT & ( copy rest of line )  1 OP +! ;

: (PRIM)   OP @ ,C  [COMPILE] EXIT  OP: ;
: PRIM   C-COMMENT  HEADER (PRIM) ;
: CODE   C-COMMENT  TARGET-CREATE (PRIM) ;

: BINARY  ( op -- )
    CREATE , IMMEDIATE  DOES> @  LATEST <LIT> =     0 AND ( FIXME)
        IF  10 + PATCH  ELSE  ,C  THEN ;

\ Target Literals
: LITERAL  ( n -- )  <LIT> ,C  ,-T ; IMMEDIATE
: $   BL WORD NUMBER DROP [COMPILE] LITERAL ; IMMEDIATE

\ Define Meta Branching Constructs
: ?CONDITION  INVERT ABORT" unbalanced" ;
: MARK  ( -- here )  HERE-T  0 ?CODE ! ;
: OFFSET  ( to from -- offset )  - ( CELL-T /) ;
: ?>MARK      ( -- f addr )   TRUE  MARK   0 ,-T ;
: ?>RESOLVE   ( f addr -- )   MARK  OVER OFFSET  SWAP !-T   ?CONDITION ;
: ?<MARK      ( -- f addr )   TRUE  MARK ;
: ?<RESOLVE   ( f addr -- )   MARK  OFFSET ,-T   ?CONDITION ;

: CONDITION  ( optimizer )
    58 ,C ;

: NOT  ( invert last conditional op )  LATEST 40 48 WITHIN
    IF  LATEST 8 + PATCH  ELSE  40 ,C  THEN ; IMMEDIATE

: IF        CONDITION  ?>MARK ; IMMEDIATE
: THEN      ?>RESOLVE ; IMMEDIATE
: ELSE      2 ,C  ?>MARK  2SWAP ?>RESOLVE ; IMMEDIATE
: BEGIN     ?<MARK ; IMMEDIATE
: UNTIL     CONDITION  ?<RESOLVE ; IMMEDIATE
: AGAIN     2 ,C  ?<RESOLVE ; IMMEDIATE
: WHILE     [COMPILE] IF  2SWAP ; IMMEDIATE
: REPEAT    [COMPILE] AGAIN  [COMPILE] THEN ; IMMEDIATE

\ Compile Strings into the Target
: STRING,-T   ( -- )
   [CHAR] " PARSE  DUP C,-T  S,-T  ALIGN  0 ?CODE ! ;

: ."      09 ,C  STRING,-T ; IMMEDIATE
: S"      0A ,C  STRING,-T ; IMMEDIATE
: ABORT"  0B ,C  STRING,-T ; IMMEDIATE

\ Defining Words
\ : EQU CONSTANT ;
: ;_  [COMPILE] ; ; IMMEDIATE

: CONSTANT  TARGET-CREATE  [COMPILE] LITERAL  [COMPILE] EXIT ;
: VARIABLE  TARGET-CREATE  8 ,C  0 ,-T ;

: T:  HEADER   0 ?CODE !  ] ;  \ to create words with no host header

: ;   [COMPILE] EXIT  [COMPILE] [ ;_ IMMEDIATE
: [COMPILE] ;_
: :   TARGET-CREATE  0 ?CODE !  ] ;_


\ **********************************************************************
\ FVM Kernel

200 DP-T !
( cold start: )  FF ,C 0 ,-T

0 OP!

OP: /* EXIT */  xit:  ip = (opcode*) *rp--; NEXT
CODE EXECUTEX       w = top; pop; goto exec;
OP: /* BRANCH */    ip += *(cell*)ip; NEXT
OP: /* DO */        NEXT
OP: /* ?DO */       NEXT
OP: /* LOOP */      NEXT
OP: /* +LOOP */     NEXT
OP: /* DLIT */      push *ip++; push *ip++; NEXT
OP: /* DOVAR */     push (uchar*)ip++ - m; goto xit;
OP: /* ." */        ip = dotq(ip); NEXT
OP: /* S" */        push (cell)ip + 1; push *(uchar*)ip; ip = litq(ip); NEXT
OP: /* abort" */    if (top) { dotq((cell*)(m + HERE)); putchar(BL); dotq(ip); goto abort; }
                    & ip = litq(ip); pop; NEXT

10 OP!

PRIM +          top += *sp--; NEXT
PRIM -          top = *sp-- - top; NEXT
PRIM AND        top &= *sp--; NEXT
PRIM OR         top |= *sp--; NEXT
PRIM XOR        top ^= *sp--; NEXT
PRIM LSHIFT     top = *sp-- << top; NEXT
PRIM RSHIFT     top = (cell) ((ucell)(*sp--) >> top); NEXT
PRIM ARSHIFT    top = *sp-- >> top; NEXT
CODE SWAP       w = top; top = *sp; *sp = w; NEXT
CODE PICK       top = sp[-top]; NEXT
CODE @          top = M(top); NEXT
CODE !          M(top) = *sp; pop2; NEXT
CODE +!         M(top) += *sp; pop2; NEXT
CODE *          top *= *sp--; NEXT
CODE /          top = *sp-- / top; NEXT
CODE NOP        NEXT

OP: ( LIT + )   top += *ip++; NEXT
OP: ( LIT - )   top -= *ip++; NEXT
OP: ( LIT AND ) top &= *ip++; NEXT

2F OP!
OP:  ( LIT )    push *(cell*)ip; ip += CELL; NEXT

40 OP!

CODE 0=         top = (top == 0) LOGICAL; NEXT
CODE 0<         top = (top < 0) LOGICAL; NEXT
CODE 0>         top = (top > 0) LOGICAL; NEXT
CODE =          top = (*sp-- == top) LOGICAL; NEXT
CODE <          top = (*sp-- < top) LOGICAL; NEXT
CODE >          top = (*sp-- > top) LOGICAL; NEXT
CODE U<         top = ((ucell)*sp-- < (ucell)top) LOGICAL; NEXT
CODE U>         top = ((ucell)*sp-- > (ucell)top) LOGICAL; NEXT

OP: ( 0<> )     top = (top != 0) LOGICAL; NEXT
OP: ( 0>= )     top = (top >= 0) LOGICAL; NEXT
OP: ( 0<= )     top = (top <= 0) LOGICAL; NEXT
OP: ( <>  )     top = (*sp-- != top) LOGICAL; NEXT
OP: ( >=  )     top = (*sp-- >= top) LOGICAL; NEXT
OP: ( <=  )     top = (*sp-- <= top) LOGICAL; NEXT
OP: ( U>= )     top = ((ucell)*sp-- >= (ucell)top) LOGICAL; NEXT
OP: ( U<= )     top = ((ucell)*sp-- <= (ucell)top) LOGICAL; NEXT

58 OP!
OP: ( 0<> IF )  ip += top ? CELL : *(cell*)ip; pop; NEXT

60 OP!
CODE DROP       pop; NEXT
CODE DUP        *++sp = top; NEXT
CODE ?DUP       if (top) *++sp = top; NEXT
OP: ( SWAP )    NEXT
CODE OVER       NEXT

: 2DROP DROP DROP ;

10 BINARY +         11 BINARY -
12 BINARY AND       13 BINARY OR        14 BINARY XOR
15 BINARY LSHIFT    16 BINARY RSHIFT    17 BINARY ARSHIFT

: 1+  $ 1 + ;
: 1-  $ 1 - ;

CODE C@  ( a -- c )  top = m[top];  NEXT
CODE C!  ( c a -- )  m[top] = *sp; pop2; NEXT
: COUNT  DUP 1+ SWAP C@ ;

CODE KEY   ( -- char )  push getchar(); NEXT
CODE EMIT  ( char -- )  putchar(top); pop; NEXT
CODE TYPE  ( a n -- )   type(*sp, top); pop2; NEXT
CODE CR    ( -- )       putchar('\n'); NEXT

CODE ACCEPT ( a n -- n )  top = accept(*sp--, top); NEXT

VARIABLE TIB 50 ALLOT-T

04 CONSTANT H
08 CONSTANT 'TIB
0C CONSTANT #TIB
10 CONSTANT >IN
14 CONSTANT CONTEXT

CODE +M  top += (cell)m; NEXT   // convert to physical address
CODE -M  top -= (cell)m; NEXT   // convert to absolute address

: QUERY  ( -- )  TIB $ 50 ACCEPT  TIB +M 'TIB !  #TIB !  $ 0 >IN ! ;

\ CODE NUMBER?  ( addr -- n f )  top = number(top, ++sp);  NEXT
\ : NUMBER  ( addr -- n )  DUP NUMBER? IF SWAP DROP ELSE
\   DROP COUNT TYPE ABORT"  ?"  THEN ;

CODE -NUMBER  ( a -- a t, n f ) w = number(top, ++sp);
&   if (w) top = 0; else *sp = top, top = -1; NEXT
\ : NUMBER  ( a -- n )  -NUMBER IF  COUNT TYPE  $ 1 ABORT"  ?"  THEN ;
: NUMBER  ( a -- n )  -NUMBER ABORT" ?" ;
    
20 CONSTANT BL
CODE WORD  ( char -- addr )  top = word(top, (Input*)(m+8), m+HERE) - m; NEXT

CODE FIND  ( str -- xt flag | str 0 )
&       w = find(top, 1);
&       if (w) *++sp = cfa(w), top = -1;
&       else push 0; NEXT

CODE -FIND  ( str v -- str t | xt f )
&       w = find(*sp, top);
&       if (w) *sp = cfa(w), top = 0;
&       else top = -1; NEXT

: -'  ( n - h t, a f )  $ 20 WORD SWAP -FIND ;
: '   ( -- a )   CONTEXT @ -' ABORT" ?" ;

CODE .  ( n -- )  printf("%d ", top); pop; NEXT
CODE DEPTH ( -- n )  w = sp - stack; push w; NEXT
CODE .S ( -- )
&       w = sp - stack;  sp[1] = top;
&       printf("[%d] ", w);
&       for (int i = 0; i < w; i++)
&           printf("%d (0x%x) ", stack[i+2], stack[i+2]);
&       NEXT

CODE WORDS  ( -- )  words(M(CONTEXT)); NEXT
CODE DUMP  ( a n -- )  dump(*sp--, top); pop; NEXT

VARIABLE STATE

: HERE  H @ ;
: ALLOT  H +! ;
: ,   H @ !   $ 4 H +! ;
: C,  H @ C!  $ 1 H +! ;

: COMPILE,  $ FF C, , ;
COMPILER
: LITERAL  $ 2F C, , ;
FORTH
CODE >R  *++rp = top, pop; NEXT
\ : EXECUTE  >R ;
CODE EXECUTE  ip = m + top, pop; NEXT

: INTERPRET  ( -- )
    BEGIN   BL WORD DUP C@
    WHILE   STATE @
        IF  $ 2 -FIND IF  $ 1 -FIND IF  NUMBER ( [COMPILE]) LITERAL
            ELSE  COMPILE,  THEN  ELSE  EXECUTE  THEN
        ELSE  $ 1 -FIND IF  NUMBER  ELSE  EXECUTE  THEN
        THEN  DEPTH 0< ABORT" stack?"
    REPEAT DROP ;

: QUIT [ HERE-T 201 !-T ]
\   ." hi" CR
\       WORDS CR
    BEGIN  CR QUERY  INTERPRET  STATE @ 0= IF  ." ok"  THEN  AGAIN ;

CODE BYE  return;

( Compiler )

CODE ALIGNED    top = aligned(top); NEXT
\ CODE ALIGN        while (HERE != aligned(HERE)) m[HERE++] = 0; NEXT
: ALIGN BEGIN HERE $ 3 AND WHILE $ 0 C, REPEAT ;
CODE CELLS      top *= CELL; NEXT
4 CONSTANT CELL
: CELL+  CELL + ;

VARIABLE LAST
: HASH  ( v -- a )  CELLS CONTEXT + ;
: HEADER  ( -- )
    ALIGN  HERE  CONTEXT @ HASH  DUP @ ,  !
    HERE LAST !  BL WORD C@ 1+ ALLOT  ALIGN ;

: PREVIOUS  ( -- a n )  CONTEXT @ HASH @  CELL+ COUNT ;
: USE  ( a -- )  PREVIOUS $ 1F AND + ALIGNED ! ;
: DOES   R> USE ;
: SMUDGE  PREVIOUS $ 20 XOR SWAP C! ;

COMPILER
: [  $ 0 STATE ! ;
: EXIT  $ 0 , ;
T: ;  SMUDGE [COMPILE] EXIT [COMPILE] [ ;
FORTH
: ]  $ -1 STATE ! ;

T: :  HEADER SMUDGE ] ;

: FORTH     $ 1 CONTEXT ! ;
: COMPILER  $ 2 CONTEXT ! ;

HERE-T 4 !-T  ( here )
14 EMPLACE  ( context )
SAVE
