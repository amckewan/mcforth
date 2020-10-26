\ fo Metacompiler
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

: `   1 PARSE WRITE  NEWLINE ;
: ``  BEGIN  REFILL 0= ABORT" missing ``"
        BL WORD COUNT S" ``" COMPARE WHILE
        SOURCE WRITE NEWLINE
      REPEAT ;

: WRITE-DICT-IMG
    S" kernel.img" R/W CREATE-FILE ?ERR
    DUP TARGET-IMAGE HERE-T ROT WRITE-FILE ?ERR
    CLOSE-FILE ?ERR ;

: WRITE-DICT-INC
    S" kernel.inc" OPEN
    BASE @ DECIMAL
    HERE-T 0 DO
        I THERE 10 0 DO
            COUNT 0 <# #S #> WRITE  S" ," WRITE
        LOOP DROP
        NEWLINE
    10 +LOOP  CLOSE  BASE ! ;

: SAVE  ( -- )
    CLOSE
    CR ." Saving " BASE @ DECIMAL HERE-T . BASE ! ." bytes..."
    \ WRITE-DICT-IMG
    WRITE-DICT-IMG
    WRITE-DICT-INC
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

    DUP 200 < ( 0 AND ( ??) IF  OP,  ELSE  FF OP, ,-T  THEN ;

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

\ Generate fo Primatives
: ?COMMENT  ( allow Forth comment after OP: etc. )
    >IN @  BL WORD COUNT S" (" COMPARE
    IF  >IN !  ELSE  DROP  [COMPILE] (  THEN ;

: C-COMMENT  S" /* " WRITE  >IN @  BL WORD COUNT WRITE  >IN !  S"  */ " WRITE ;
VARIABLE OP  ( next opcode )
: OP!  OP ! ;
: OP:  ( output opcode case statement )
    S" case 0x" WRITE  OP @ 0 <# # # # #> WRITE  S" :  " WRITE
    ?COMMENT ` ( copy rest of line )  1 OP +! ;

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
: >MARK      ( -- f addr )   TRUE  MARK   0 ,-T ;
: >RESOLVE   ( f addr -- )   MARK  OVER OFFSET  SWAP !-T   ?CONDITION ;
: <MARK      ( -- f addr )   TRUE  MARK ;
: <RESOLVE   ( f addr -- )   MARK  OFFSET ,-T   ?CONDITION ;

: CONDITION  ( optimizer )
    58 OP, ;

: NOT  ( invert last conditional op )  LATEST 40 48 WITHIN
    IF  LATEST 8 + PATCH  ELSE  40 OP,  THEN ; IMMEDIATE

: IF        CONDITION  >MARK ; IMMEDIATE
: THEN      >RESOLVE ; IMMEDIATE
: ELSE      2 OP,  >MARK  2SWAP >RESOLVE ; IMMEDIATE
: BEGIN     <MARK ; IMMEDIATE
: UNTIL     CONDITION  <RESOLVE ; IMMEDIATE
: AGAIN     2 OP,  <RESOLVE ; IMMEDIATE
: WHILE     [COMPILE] IF  2SWAP ; IMMEDIATE
: REPEAT    [COMPILE] AGAIN  [COMPILE] THEN ; IMMEDIATE

: DO        3 OP,  >MARK  <MARK ; IMMEDIATE
: ?DO       4 OP,  >MARK  <MARK ; IMMEDIATE
: LOOP      5 OP,  <RESOLVE  >RESOLVE ; IMMEDIATE
: +LOOP     6 OP,  <RESOLVE  >RESOLVE ; IMMEDIATE

\ Compile Strings into the Target
: C"  HERE-T  [CHAR] " PARSE S,-T  0 C,-T ; \ c-style string
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
\ fo Kernel

``
#define push *--S = top, top =
#define pop top = *S++
#define pop2 top = S[1], S += 2
#define pop3 top = S[2], S += 3
#define LOGICAL ? -1 : 0
#define NEXT ; goto next;
``

200 DP-T !
\ ( cold start: )  FF OP, 0 ,-T

0 OP!

OP: /* EXIT */  Exit:  I = (opcode*) *R++; NEXT
CODE EXECUTEX       w = top; pop; goto exec;

` #define OFFSET    *(cell*)I
` #define BRANCH    I += OFFSET
` #define NOBRANCH  I += CELL

OP: /* BRANCH */    BRANCH; NEXT
OP: /* DO */        *--R = (cell)I + OFFSET, *--R = *S, *--R = top - *S++, pop;
`                   //printf("DO R=%p I=%d %d\n", R, R[0], R[1]);
`                   NOBRANCH; NEXT
OP: /* ?DO */       if (top == *S) BRANCH;
`                   else *--R = (cell)I + OFFSET,
`                       *--R = *S, *--R = top - *S, NOBRANCH;
`                   S++, pop; NEXT
OP: /* LOOP */      //printf("LOOP R=%p I=%d %d\n", R, R[0], R[1]);
`                   if ((++ *R) == 0) NOBRANCH, R += 3;
`                   else BRANCH; NEXT
OP: /* +LOOP */     w = *R, *R += top;
`                   if ((w ^ *R) < 0 && (w ^ top) < 0) NOBRANCH, R += 3;
`                   else BRANCH; pop; NEXT

OP: /* DLIT */      push *I++; push *I++; NEXT
( DOVAR must be 8!)
OP: /* DOVAR */     push (uchar*)I++ - m; goto Exit;
OP: /* ." */        I = dotq(I); NEXT
OP: /* S" */        push rel(I) + 1; push *I; I = litq(I); NEXT
OP: /* abort" */    if (top) {
                    `   show_error((char*)I, abs(HERE), abs(SOURCE));
                    `   goto abort;
                    ` } I = litq(I); pop; NEXT

10 OP!

PRIM +          top += *S++; NEXT
PRIM -          top = *S++ - top; NEXT
PRIM AND        top &= *S++; NEXT
PRIM OR         top |= *S++; NEXT
PRIM XOR        top ^= *S++; NEXT
PRIM LSHIFT     top = *S++ << top; NEXT
PRIM RSHIFT     top = ((ucell)*S++) >> top; NEXT
CODE SWAP       w = top; top = *S; *S = w; NEXT

CODE PICK       top = S[top]; NEXT
CODE @          top = *(cell *)(m + top); NEXT
CODE !          *(cell *)(m + top) = *S; pop2; NEXT
CODE +!         *(cell *)(m + top) += *S; pop2; NEXT
CODE *          top *= *S++; NEXT
CODE /          top = *S++ / top; NEXT
CODE NOP        NEXT

OP: ( LIT + )   top += *I++; NEXT
OP: ( LIT - )   top -= *I++; NEXT
OP: ( LIT AND ) top &= *I++; NEXT

2F OP!
OP:  ( LIT )    push *(cell*)I; I += CELL; NEXT

40 OP!

CODE 0=         top = (top == 0) LOGICAL; NEXT
CODE 0<         top = (top < 0) LOGICAL; NEXT
CODE 0>         top = (top > 0) LOGICAL; NEXT
CODE =          top = (*S++ == top) LOGICAL; NEXT
CODE <          top = (*S++ < top) LOGICAL; NEXT
CODE >          top = (*S++ > top) LOGICAL; NEXT
CODE U<         top = ((ucell)*S++ < (ucell)top) LOGICAL; NEXT
CODE U>         top = ((ucell)*S++ > (ucell)top) LOGICAL; NEXT

OP: /* 0<> */   top = (top != 0) LOGICAL; NEXT
OP: /* 0>= */   top = (top >= 0) LOGICAL; NEXT
OP: /* 0<= */   top = (top <= 0) LOGICAL; NEXT
OP: /* <>  */   top = (*S++ != top) LOGICAL; NEXT
OP: /* >=  */   top = (*S++ >= top) LOGICAL; NEXT
OP: /* <=  */   top = (*S++ <= top) LOGICAL; NEXT
OP: /* U>= */   top = ((ucell)*S++ >= (ucell)top) LOGICAL; NEXT
OP: /* U<= */   top = ((ucell)*S++ <= (ucell)top) LOGICAL; NEXT

50 OP!  ( cond IF, must be in the same order as above )

` #define IF(cond)  if (cond) NOBRANCH; else BRANCH
` #define IF1(cond) IF(cond); pop; NEXT
` #define IF2(cond) IF(cond); pop2; NEXT

OP: /* 0= IF */     IF1(top == 0)
OP: /* 0< IF */     IF1(top < 0)
OP: /* 0> IF */     IF1(top > 0)
OP: /* = IF */      IF2(*S == top)
OP: /* < IF */      IF2(*S < top)
OP: /* > IF */      IF2(*S > top)
OP: /* U< IF */     IF2((ucell)*S < (ucell)top)
OP: /* U> IF */     IF2((ucell)*S > (ucell)top)

OP: /* 0<> IF */    IF1(top != 0)
OP: /* 0>= IF */    IF1(top >= 0)
OP: /* 0<= IF */    IF1(top <= 0)
OP: /* <> IF */     IF2(*S != top)
OP: /* >= IF */     IF2(*S >= top)
OP: /* <= IF */     IF2(*S <= top)
OP: /* U>= IF */    IF2((ucell)*S >= (ucell)top)
OP: /* U<= IF */    IF2((ucell)*S <= (ucell)top)

60 OP!
CODE DROP       pop; NEXT
CODE DUP        *--S = top; NEXT
CODE NIP        S++; NEXT
CODE ?DUP       if (top) *--S = top; NEXT
\ OP: ( SWAP )  ; NEXT
CODE OVER       push S[1]; NEXT
CODE ROT        w = S[1], S[1] = *S, *S = top, top = w; NEXT

68 OP!
\ 68-6F must be inlined
CODE >R         *--R = top, pop; NEXT
CODE R>         push *R++; NEXT
CODE R@         push *R  ; NEXT


CODE I          push R[0] + R[1]; NEXT
CODE J          push R[3] + R[4]; NEXT
CODE LEAVE      I = (byte*)R[2];
CODE UNLOOP     R += 3; NEXT

: 2DUP      OVER OVER ;
: 2DROP     DROP DROP ;

70 OP!
10 BINARY +         11 BINARY -
12 BINARY AND       13 BINARY OR        14 BINARY XOR
15 BINARY LSHIFT    16 BINARY RSHIFT    17 BINARY ARSHIFT

CODE INVERT  top = ~top; NEXT
CODE NEGATE  top = -top; NEXT

CODE MOD  top = *S++ % top;  NEXT

` #define LOWER(u1,u2)  ((uint32_t)(u1) < (uint32_t)(u2))

CODE WITHIN
`   w = *S++,
`   top = LOWER(*S - w, top - w) LOGICAL;
`   S++;
`   NEXT

CODE M*  ( n1 n2 -- d ) {
`   int64_t d = (int64_t)*S * (int64_t)top;
`   *S = d ;
`   top = d >> 32;
`   NEXT }

CODE UM* ( u1 u2 -- ud ) {
`   uint64_t u1 = (uint32_t)*S;
`   uint64_t u2 = (uint32_t)top;
`   uint64_t ud = u1 * u2;
`   *S = ud ;
`   top = ud >> 32;
`   NEXT }

CODE UM/MOD  ( ud u1 -- rem quot ) {
`   uint64_t ud = ((uint64_t)*S << 32) | (uint32_t)S[1];
`   uint64_t u = (uint32_t)top;
`   uint32_t quot = ud / u;
`   uint32_t rem = ud % u;
`   *++S = rem;
`   top = quot;
`   NEXT }

CODE SM/REM  ( d n -- rem quot ) {
`   int64_t d = (((uint64_t)*S) << 32) | ((uint32_t) S[1]);
`   int32_t quot = d / top;
`   int32_t rem = d % top;
`   *++S = rem;
`   top = quot;
`   NEXT }

CODE 1+     top += 1; NEXT
CODE 1-     top -= 1; NEXT
CODE 2*     top <<= 1; NEXT
CODE 2/     top >>= 1; NEXT

CODE CELLS      top *= CELL; NEXT
CELL-T CONSTANT CELL
: CELL+  CELL + ;

CODE C@  ( a -- c )  top = m[top]; NEXT
CODE C!  ( c a -- )  m[top] = *S; pop2; NEXT
: COUNT  DUP 1+ SWAP C@ ;

: 2@    DUP CELL+ @ SWAP @ ;
: 2!    DUP >R ! R> CELL+ ! ;

CODE FILL  ( a u c -- )  memset(abs(S[1]), top, *S); pop3; NEXT
CODE MOVE  ( src dest u -- )  memmove(abs(*S), abs(S[1]), top); pop3; NEXT

CODE >ABS  top += (cell)m; NEXT   // convert to absolute address
CODE >REL  top -= (cell)m; NEXT   // convert to relative address

CODE KEY   ( -- char )  push getchar(); NEXT
CODE EMIT  ( char -- )  putchar(top); pop; NEXT
CODE TYPE  ( a n -- )   type(*S, top); pop2; NEXT

\ : X $ 12345 >R KEY DROP R> KEY DROP ;

: CR     $ 0A EMIT ;
: SPACE  $ 20 EMIT ;

CODE BYE  return;

CODE ACCEPT ( a n -- n )  top = accept(*S++, top);
` /* FIXME */ if (top < 0) exit(0); NEXT


\ Variables shared with C code at fixed offsets
04 CONSTANT H
08 CONSTANT 'SOURCE
0C CONSTANT BASE
10 CONSTANT STATE
14 CONSTANT CONTEXT

CODE ARGC ( -- n ) push argc; NEXT
CODE ARGV ( n -- a n ) *--S = rel(argv[top]); top = (cell)strlen(argv[top]); NEXT


( ********** File I/O ********** )

C" r"  CONSTANT R/O
C" w"  CONSTANT W/O
C" w+" CONSTANT R/W

CODE CREATE-FILE ( c-addr u fam -- fileid ior )
    ` top = (cell)open_file(abs(S[1]), *S, abs(top));
    ` *++S = top, top = top ? 0 : -1; NEXT

CODE OPEN-FILE ( c-addr u fam -- fileid ior )
    ` top = (cell)open_file(abs(S[1]), *S, abs(top));
    ` *++S = top, top = top ? 0 : -1; NEXT

CODE CLOSE-FILE ( fileid -- ior )
    ` top = fclose((FILE*)top); NEXT

CODE READ-FILE ( a u fid -- u' ior )
    ` w = fread(abs(S[1]), 1, *S, (FILE*)top);
    ` top = w == *S ? 0 : ferror((FILE*)top); *++S = w; NEXT

CODE READ-LINE ( a u fid -- u' flag ior )
    ` w = (cell)fgets(abs(S[1]), *S + 1, (FILE*)top);
    ` if (!w) {
    `   top = feof((FILE*)top) ? 0 : ferror((FILE*)top);
    `   *S = S[1] = 0; NEXT
    ` }
    ` top = strlen((char*)w);
    ` if (top > 0 && ((char*)w)[top-1] == '\n') --top;
    ` S[1] = top, *S = TRUE, top = 0; NEXT

CODE WRITE-FILE ( a u fid -- ior )
    ` w = fwrite(abs(S[1]), 1, *S, (FILE*)top);
    ` top = w == *S ? 0 : ferror((FILE*)top); S += 2; NEXT

CODE WRITE-LINE ( a u fid -- ior )
    ` w = fwrite(abs(S[1]), 1, *S, (FILE*)top);
    ` if (w == *S) *S = 1, w = fwrite("\n", 1, 1, (FILE*)top);
    ` top = w == *S ? 0 : ferror((FILE*)top); S += 2; NEXT


( ********** Input source processig ********** )

\ 8 CONSTANT SOURCE-CELLS ( sizeof )

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

CODE ALLOCATE   *--S = rel(malloc(top)), top = *S ? 0 : -1; NEXT
CODE RESIZE     *S = rel(realloc(abs(*S), top)), top = *S ? 0 : -1; NEXT
CODE FREE       if (top) free(abs(top)); top = 0; NEXT

CODE NEW-STRING top = new_string(*S++, top); NEXT

: FILE?  1+ $ 2 U< NOT ;

: >SOURCE ( filename len fileid | -1 -- ) \ CR ." Including " DROP TYPE SPACE ;
    SOURCE-DEPTH $ 7 U> ABORT" nested too deep"
    $ 20 'SOURCE +!
    DUP 'SOURCE-ID !
    FILE? IF  $ 80 ALLOCATE DROP SOURCE-BUF !  NEW-STRING SOURCE-NAME !  THEN
    $ 0 SOURCE-LINE ! ;

: SOURCE> ( -- )
    SOURCE-DEPTH $ 1 < ABORT" trying to pop empty source"
    SOURCE-ID FILE? IF
        SOURCE-ID CLOSE-FILE DROP
        SOURCE-BUF @ FREE DROP
        SOURCE-NAME @ FREE DROP
    THEN
    $ -20 'SOURCE +! ;

CODE REFILL ( -- f )  push refill(SOURCE); NEXT

VARIABLE TIB 80 ALLOT-T
: QUERY  ( -- )  $ 0 'SOURCE-ID !  TIB SOURCE-BUF !  REFILL 0= IF BYE THEN ;

\ ********** Numbers **********

CODE .  ( n -- )  printf("%d ", top); pop; NEXT
: ?  @ . ;

CODE -NUMBER  ( a -- a t, n f ) w = number(abs(top), --S);
`   if (w) top = 0; else *S = top, top = -1; NEXT
: NUMBER  ( a -- n )  -NUMBER ABORT" ? " ;

CODE >NUMBER  top = to_number(S, top); NEXT

20 CONSTANT BL

CODE WORD  ( char -- addr )
`   top = word(SOURCE, top, HERE); NEXT

CODE FIND  ( str -- xt flag | str 0 )
`       w = find(top, 1);
`       if (w) *--S = cfa(w), top = -1;
`       else push 0; NEXT

CODE -FIND  ( str v -- str t | xt f )
`       w = find(*S, top);
`       if (w) *S = cfa(w), top = 0;
`       else top = -1; NEXT

: -'  ( n - h t, a f )  $ 20 WORD SWAP -FIND ;
: '   ( -- a )   CONTEXT @ -' ABORT" ?" ;

CODE DEPTH ( -- n )  w = S0 - S; push w; NEXT
CODE .S ( -- )
`       w = S0 - S;  S[-1] = top;
`       printf("[%d] ", w);
`       for (int i = w - 2; i >= -1; i--)
`           printf("%d (0x%x) ", S[i], S[i]);
`       NEXT


CODE WORDS  ( -- )  words(M[CONTEXT]); NEXT
CODE DUMP  ( a n -- )  dump(*S++, top); pop; NEXT

CODE LIMIT  push sizeof m; NEXT

: HERE  H @ ;
: ALLOT  H +! ;
: ,   H @ !   $ 4 H +! ;
: C,  H @ C!  $ 1 H +! ;

( ********** Interpreter ********** )

: COMPILE,  ( xt -- )
    DUP C@ $ 68 $ 70 WITHIN OVER 1+ C@ 0= AND IF  C@ C, EXIT  THEN
    $ FF C, , ;
COMPILER
: LITERAL  $ 2F C, , ;
FORTH
: EXECUTE  >ABS >R ;
\ CODE EXECUTE  I = m + top, pop; NEXT

: INTERPRET  ( -- )
    BEGIN   BL WORD DUP C@
    WHILE   STATE @
        IF    $ 2 -FIND IF  $ 1 -FIND IF  NUMBER  [COMPILE] LITERAL
              ELSE  COMPILE,  THEN  ELSE  EXECUTE  THEN
        ELSE  $ 1 -FIND IF  NUMBER  ELSE  EXECUTE  THEN
        THEN  DEPTH 0< ABORT" stack? "
    REPEAT DROP ;

: QUIT [ HERE-T 20 !-T ]
    BEGIN SOURCE-DEPTH 0> WHILE SOURCE> REPEAT
    \ RP0 @ RP!
\   ." hi" CR
\       WORDS CR
    BEGIN  CR QUERY  INTERPRET  STATE @ 0= IF  ."  ok"  THEN  AGAIN ;

: INCLUDED  ( str len -- )
    2DUP R/O OPEN-FILE ABORT" file not found"
    >SOURCE  BEGIN REFILL WHILE INTERPRET REPEAT  SOURCE> ;

: INCLUDE  BL WORD COUNT INCLUDED ;

: TEST S" test.fs" INCLUDED ;

: BOOT  [ HERE-T 0 !-T ]
    ARGC $ 1 ?DO  I ARGV INCLUDED  LOOP
    ." Hello" QUIT ;

( ********** Compiler ********** )

: OP,  C, ;

CODE ALIGNED    top = aligned(top); NEXT
\ CODE ALIGN        while (HERE != aligned(HERE)) m[HERE++] = 0; NEXT
: ALIGN BEGIN HERE $ 3 AND WHILE $ 0 C, REPEAT ;

CODE PARSE  ( c -- a n )  top = parse(SOURCE, top, --S); NEXT

: S,  ( a n -- )  BEGIN DUP WHILE >R COUNT C, R> 1- REPEAT 2DROP ;
: ",  $ 22 ( [CHAR] ") PARSE  DUP C,  S,  ALIGN  ( 0 ?CODE !) ;

VARIABLE WARNING
: WARN  WARNING @ IF  >IN @  BL WORD CONTEXT @ -FIND 0= IF
    HERE COUNT TYPE ."  redefined " THEN  DROP >IN !  THEN ;

VARIABLE LAST ( nfa)
: HASH  ( v -- a )  CELLS CONTEXT + ;
: HEADER  ( -- )  WARN
    ALIGN  HERE  CONTEXT @ HASH  DUP @ ,  !
    HERE LAST !  BL WORD C@ 1+ ALLOT  ALIGN ;

: CONSTANT  HEADER  [COMPILE] LITERAL  $ 0 OP, ;
: VARIABLE  HEADER  $ 8 OP,  $ 0 , ;

\ | opc | I for does | data
\ OP: /* DOVAR */     push (uchar*)I++ - m; goto Exit;
F0 OP!
OP: /* docreate */
` push I + CELL - m;
` w = *(cell*)I;
` if (!w) goto Exit; I = abs(w); NEXT

\ TODO: move me!
FF OP!
OP: /* call */
    ` w = *(cell*)I;
    ` //printf("call 0x%x\n", w);
    ` *--R = (cell)I + CELL;
    ` I = (opcode*)(m + w);
    ` NEXT

: CREATE  HEADER $ F0 C, $ 0 , ;

: PREVIOUS  ( -- nfa count )  CONTEXT @ HASH @  CELL+ DUP C@ ;
: DOES>   R> >REL  PREVIOUS $ 1F AND + 1+ ALIGNED 1+ ! ;
: SMUDGE  PREVIOUS $ 20 XOR SWAP C! ;

\ Be careful from here on...

COMPILER
: [  $ 0 STATE ! ;
: EXIT  $ 0 OP, ;
T: ;  SMUDGE  [COMPILE] EXIT  [COMPILE] [ ;
: [COMPILE]  $ 2 -' ABORT" ?" COMPILE, ;

: ."      $ 9 OP,  ", ;
: S"      $ A OP,  ", ;
: ABORT"  $ B OP,  ", ;
FORTH

: ]  $ -1 STATE ! ;
T: :  HEADER SMUDGE ] ;

: FORTH     $ 1 CONTEXT ! ;
: COMPILER  $ 2 CONTEXT ! ;

HERE-T 4 !-T  ( here )
14 EMPLACE  ( context )
SAVE
