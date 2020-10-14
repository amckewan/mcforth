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
: |  1 PARSE WRITE  NEWLINE ;

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
    CR ." Saving " BASE @ DECIMAL HERE-T . BASE ! ." bytes..."
    \ WRITE-DICT-IMG
    WRITE-DICT
    WRITE-DICT-DUMP
    ." done" ;

: ciao cr bye ;

\ **********************************************************************
\ Compiler

\ Opcodes
2F CONSTANT <LIT>

VARIABLE ?CODE
: LATEST  ( -- n )  ?CODE @ C@-T ;
: PATCH  ( n -- )  ?CODE @ C!-T ;
: OP,  ( opcode -- )  HERE-T ?CODE !  C,-T ;
\ : ,A  ( addr -- )   OP, ;

: COMPILE,  ( op/addr -- )
    DUP C@-T 68 70 WITHIN OVER 1+ C@-T 0= AND IF  C@-T OP, EXIT  THEN

    DUP 200 < IF  OP,  ELSE  FF OP, ,-T  THEN ;

\   OP, ;  ( non-optimizing )
\   DUP 200 < IF  OP,  ELSE  DUP @-T  ( adr op )
\   OVER 2 CELLS + @-T 0=  OVER 20 40 WITHIN AND
\   IF  ( literal ) OP,  CELL+ @-T ,-T  ELSE
\   OVER CELL+ @-T 0=  SWAP 200 < AND
\   IF  ( primative )  @-T OP,  ELSE
\   ( call )  ,A  THEN THEN THEN ;

: EXIT  0 OP, ; IMMEDIATE

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
   DOES>  STATE @ 0= ABORT" target word!"  @ COMPILE, ;

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
    ?COMMENT | ( copy rest of line )  1 OP +! ;

: (PRIM)   OP @ OP,  [COMPILE] EXIT  OP: ;
: PRIM   C-COMMENT  HEADER (PRIM) ;  ( in target only )
: CODE   C-COMMENT  TARGET-CREATE (PRIM) ;  ( in host and target)

: BINARY  ( op -- )
    CREATE , IMMEDIATE  DOES> @ OP, ;
\    CREATE , IMMEDIATE  DOES> @  LATEST <LIT> =
\        IF  10 + PATCH  ELSE  OP,  THEN ;

\ Target Literals
: LITERAL  ( n -- )  <LIT> OP,  ,-T ; IMMEDIATE
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
    58 OP, ;

: NOT  ( invert last conditional op )  LATEST 40 48 WITHIN
    IF  LATEST 8 + PATCH  ELSE  40 OP,  THEN ; IMMEDIATE

: IF        CONDITION  ?>MARK ; IMMEDIATE
: THEN      ?>RESOLVE ; IMMEDIATE
: ELSE      2 OP,  ?>MARK  2SWAP ?>RESOLVE ; IMMEDIATE
: BEGIN     ?<MARK ; IMMEDIATE
: UNTIL     CONDITION  ?<RESOLVE ; IMMEDIATE
: AGAIN     2 OP,  ?<RESOLVE ; IMMEDIATE
: WHILE     [COMPILE] IF  2SWAP ; IMMEDIATE
: REPEAT    [COMPILE] AGAIN  [COMPILE] THEN ; IMMEDIATE

\ Compile Strings into the Target
: ",  [CHAR] " PARSE  DUP C,-T  S,-T  ALIGN  0 ?CODE ! ;

: ."      09 OP,  ", ; IMMEDIATE
: S"      0A OP,  ", ; IMMEDIATE
: ABORT"  0B OP,  ", ; IMMEDIATE

\ Defining Words
\ : EQU CONSTANT ;
: ;_  [COMPILE] ; ; IMMEDIATE

: CONSTANT  TARGET-CREATE  [COMPILE] LITERAL  [COMPILE] EXIT ;
: VARIABLE  TARGET-CREATE  8 OP,  0 ,-T ;

: T:  HEADER   0 ?CODE !  ] ;  \ to create words with no host header

: ;   [COMPILE] EXIT  [COMPILE] [ ;_ IMMEDIATE
: [COMPILE] ;_
: :   TARGET-CREATE  0 ?CODE !  ] ;_


\ **********************************************************************
\ FVM Kernel

200 DP-T !
( cold start: )  FF OP, 0 ,-T

| #define S sp
| #define R rp
| #define I ip

0 OP!

OP: /* EXIT */  xit:  ip = (opcode*) *rp--; NEXT
CODE EXECUTEX       w = top; pop; goto exec;
OP: /* BRANCH */    ip += *(cell*)ip; NEXT
OP: /* DO */        ; NEXT // TODO
OP: /* ?DO */       ; NEXT // TODO
OP: /* LOOP */      ; NEXT // TODO
OP: /* +LOOP */     ; NEXT // TODO
OP: /* DLIT */      push *ip++; push *ip++; NEXT
( DOVAR must be 8!)
OP: /* DOVAR */     push (uchar*)ip++ - m; goto xit;
OP: /* ." */        ip = dotq(ip); NEXT
OP: /* S" */        push virt(ip) + 1; push *ip; ip = litq(ip); NEXT
OP: /* abort" */    if (top) {
                    |   dotq(m + HERE); putchar(BL); dotq(ip);
                    |   goto abort;
                    | } ip = litq(ip); pop; NEXT

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
CODE NOP      ; NEXT

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

OP: /* 0<> */   top = (top != 0) LOGICAL; NEXT
OP: /* 0>= */   top = (top >= 0) LOGICAL; NEXT
OP: /* 0<= */   top = (top <= 0) LOGICAL; NEXT
OP: /* <>  */   top = (*sp-- != top) LOGICAL; NEXT
OP: /* >=  */   top = (*sp-- >= top) LOGICAL; NEXT
OP: /* <=  */   top = (*sp-- <= top) LOGICAL; NEXT
OP: /* U>= */   top = ((ucell)*sp-- >= (ucell)top) LOGICAL; NEXT
OP: /* U<= */   top = ((ucell)*sp-- <= (ucell)top) LOGICAL; NEXT

58 OP!
OP: ( 0<> IF )  ip += top ? CELL : *(cell*)ip; pop; NEXT

60 OP!
CODE DROP       pop; NEXT
CODE DUP        *++sp = top; NEXT
CODE NIP        sp--; NEXT
CODE ?DUP       if (top) *++sp = top; NEXT
\ OP: ( SWAP )  ; NEXT
CODE OVER       push sp[-1]; NEXT
CODE ROT        w = S[-1], S[-1] = *S, *S = top, top = w; NEXT

68 OP!
\ 68-6F must be inlined
CODE >R         
|    //printf(">R R=%p top=0x%X", R, top);getchar();
|    *++rp = top, pop;
|    //printf(">R R=%p *R=0x%X", R, *R);getchar();
|  ; NEXT
CODE R>         
|    //printf("R> R=%p *R=0x%X", R, *R);getchar();
|    push *rp--; 
|    //printf("R> R=%p top=0x%X", R, top);getchar();
|  ; NEXT
CODE R@         push *rp  ; NEXT
70 OP!

: 2DUP      OVER OVER ;
: 2DROP     DROP DROP ;

10 BINARY +         11 BINARY -
12 BINARY AND       13 BINARY OR        14 BINARY XOR
15 BINARY LSHIFT    16 BINARY RSHIFT    17 BINARY ARSHIFT

: 1+  $ 1 + ;
: 1-  $ 1 - ;

CODE CELLS      top *= CELL; NEXT
CELL-T CONSTANT CELL
: CELL+  CELL + ;

CODE C@  ( a -- c )  top = m[top]; NEXT
CODE C!  ( c a -- )  m[top] = *sp; pop2; NEXT
: COUNT  DUP 1+ SWAP C@ ;

: 2@    DUP CELL+ @ SWAP @ ;
: 2!    DUP >R ! R> CELL+ ! ;

CODE PHYS  top += (cell)m; NEXT   // convert to physical address
CODE VIRT  top -= (cell)m; NEXT   // convert to absolute address

CODE KEY   ( -- char )  push getchar(); NEXT
CODE EMIT  ( char -- )  putchar(top); pop; NEXT
CODE TYPE  ( a n -- )   type(*sp, top); pop2; NEXT

\ : X $ 12345 >R KEY DROP R> KEY DROP ;

: CR     $ 0A EMIT ;
: SPACE  $ 20 EMIT ;

CODE .  ( n -- )  printf("%d ", top); pop; NEXT
CODE H. ( u -- )  printf("0x%X ", top); pop; NEXT

CODE BYE  return;

CODE ACCEPT ( a n -- n )  top = accept(*sp--, top);
| /* FIXME */ if (top < 0) exit(0); NEXT


04 CONSTANT H
14 CONSTANT CONTEXT

| #define SOURCE M(8)

\ 8 CONSTANT SOURCE-CELLS ( sizeof )

8 CONSTANT 'SOURCE
ALIGN  HERE-T 8 !-T  HERE-T 100 ( 20 8 *) ALLOT-T
CONSTANT SOURCE-STACK

: >IN           'SOURCE @ ;
: #TIB          >IN     CELL+ ;
: 'TIB          >IN $ 2 CELLS + ;
: 'SOURCE-ID    >IN $ 3 CELLS + ;
: SOURCE-BUF    >IN $ 4 CELLS + ;
: SOURCE-NAME   >IN $ 5 CELLS + ;
: SOURCE-LINE   >IN $ 6 CELLS + ;

: SOURCE        >IN CELL+ 2@ ;
: SOURCE-ID     'SOURCE-ID @ ;

: SOURCE-DEPTH  >IN SOURCE-STACK -  $ 5 RSHIFT ( 32 /) ;

CODE ALLOCATE   *++S = virt(malloc(top)), top = *S ? 0 : -1; NEXT
CODE RESIZE     *S = virt(realloc(phys(*S), top)), top = *S ? 0 : -1; NEXT
CODE FREE       free(phys(top)), top = 0; NEXT

CODE NEW-STRING top = new_string(*sp--, top); NEXT

CODE OPEN-FILE ( c-addr u fam -- fileid ior ) {
|   //printf("open a=0x%x, u=%d, fam=%d\n", S[-1], *S, top);
|   char *filename = "meta.fs"; // phys(new_string(S[-1], *S));
|   FILE *file = fopen(filename, "r");
|   printf("open %s returned %p\n", filename, file);
|   *--S = (cell) file;
|   top = file ? 0 : -1; NEXT }

CODE CLOSE-FILE ( fileid -- ior )  
|   printf("closing %p\n", (FILE*)top);
|   top = fclose((FILE*)top); NEXT

: FILE?  1+ $ 2 U< NOT ;

: >SOURCE ( str len fileid -- ) \ CR ." Including " DROP TYPE SPACE ;
    SOURCE-DEPTH $ 7 U> ABORT" nested too deep"
    $ 20 'SOURCE +!
    DUP 'SOURCE-ID !  FILE? IF
        $ 80 ALLOCATE DROP SOURCE-BUF !
        NEW-STRING SOURCE-NAME !
    ELSE
        $ 0 DUP SOURCE-BUF ! SOURCE-NAME !
    THEN
    $ 0 SOURCE-LINE ! ;

: SOURCE> ( -- )
    SOURCE-DEPTH $ 1 < ABORT" trying to pop empty source"
    SOURCE-ID FILE? IF
        SOURCE-ID CLOSE-FILE DROP
        SOURCE-BUF @ FREE DROP
        SOURCE-NAME @ FREE DROP
    THEN
    $ -20 'SOURCE +! 
    ;


CODE REFILL ( -- f )  push refill(SOURCE); NEXT

VARIABLE TIB 80 ALLOT-T
: QUERY  ( -- )  TIB SOURCE-BUF !  REFILL 0= IF BYE THEN ;




\ CODE NUMBER?  ( addr -- n f )  top = number(top, ++sp);; NEXT
\ : NUMBER  ( addr -- n )  DUP NUMBER? IF SWAP DROP ELSE
\   DROP COUNT TYPE ABORT"  ?"  THEN ;

CODE -NUMBER  ( a -- a t, n f ) w = number(top, ++sp);
|   if (w) top = 0; else *sp = top, top = -1; NEXT
\ : NUMBER  ( a -- n )  -NUMBER IF  COUNT TYPE  $ 1 ABORT"  ?"  THEN ;
: NUMBER  ( a -- n )  -NUMBER ABORT" ?" ;
    
20 CONSTANT BL

CODE WORD  ( char -- addr )
|   top = word(SOURCE, top, HERE); NEXT

CODE FIND  ( str -- xt flag | str 0 )
|       w = find(top, 1);
|       if (w) *++sp = cfa(w), top = -1;
|       else push 0; NEXT

CODE -FIND  ( str v -- str t | xt f )
|       w = find(*sp, top);
|       if (w) *sp = cfa(w), top = 0;
|       else top = -1; NEXT

: -'  ( n - h t, a f )  $ 20 WORD SWAP -FIND ;
: '   ( -- a )   CONTEXT @ -' ABORT" ?" ;

CODE DEPTH ( -- n )  w = sp - stack; push w; NEXT
CODE .S ( -- )
|       w = sp - stack;  sp[1] = top;
|       printf("[%d] ", w);
|       for (int i = 0; i < w; i++)
|           printf("%d (0x%x) ", stack[i+2], stack[i+2]);
|     ; NEXT


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
: EXECUTE  PHYS >R ;
\ CODE EXECUTE  ip = m + top, pop; NEXT

: INTERPRET  ( -- )
    \ BEGIN BL WORD DUP C@ WHILE COUNT TYPE SPACE REPEAT EXIT
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
    BEGIN  CR QUERY  INTERPRET  STATE @ 0= IF  ."  ok"  THEN  AGAIN ;

: INCLUDED  ( str len -- )
    \ $ 0 OPEN-FILE .S EXIT ABORT" file not found" DROP EXIT
    2DUP $ 0 OPEN-FILE ABORT" file not found"
    >SOURCE  BEGIN REFILL WHILE ( INTERPRET) REPEAT  SOURCE> ;

: INCLUDE  BL WORD COUNT INCLUDED ;

: xx S" meta.fs" INCLUDED ;

( Compiler )
: OP,  C, ;

CODE ALIGNED    top = aligned(top); NEXT
\ CODE ALIGN        while (HERE != aligned(HERE)) m[HERE++] = 0; NEXT
: ALIGN BEGIN HERE $ 3 AND WHILE $ 0 C, REPEAT ;

CODE PARSE  ( c -- a n )  top = parse(SOURCE, top, ++S); NEXT

: S,  ( a n -- )  BEGIN DUP WHILE >R COUNT C, R> 1- REPEAT 2DROP ;
: ",  $ 22 ( [CHAR] ") PARSE  DUP C,  S,  ALIGN  ( 0 ?CODE !) ;

COMPILER
: test ." testing... " ;
: ."      $ 9 OP,  ", ;
: S"      $ A OP,  ", ;
: ABORT"  $ B OP,  ", ;
FORTH

VARIABLE LAST
: HASH  ( v -- a )  CELLS CONTEXT + ;
: HEADER  ( -- )
    ALIGN  HERE  CONTEXT @ HASH  DUP @ ,  !
    HERE LAST !  BL WORD C@ 1+ ALLOT  ALIGN ;

: CONSTANT  HEADER  ( [COMPILE]) LITERAL  $ 0 OP, ;
: CREATE    HEADER  $ 8 OP, ;
: VARIABLE  CREATE $ 0 , ;

: PREVIOUS  ( -- nfa count )  CONTEXT @ HASH @  CELL+ DUP C@ ;
\ : USE  ( a -- )  PREVIOUS $ 1F AND + ALIGNED ! ;
\ : DOES   R> USE ;
: SMUDGE  PREVIOUS $ 20 XOR SWAP C! ;

COMPILER
: [  $ 0 STATE ! ;
: EXIT  $ 0 OP, ;
T: ;  SMUDGE [COMPILE] EXIT [COMPILE] [ ;
FORTH
: ]  $ -1 STATE ! ;

T: :  HEADER SMUDGE ] ;

: FORTH     $ 1 CONTEXT ! ;
: COMPILER  $ 2 CONTEXT ! ;

HERE-T 4 !-T  ( here )
14 EMPLACE  ( context )
SAVE
