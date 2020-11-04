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
: W!-T    ( w taddr -- )      THERE OVER 8 RSHIFT OVER 1+ C! C! ; ( le )
: !-T     ( n taddr -- )      THERE CELL! ;
: HERE-T  ( -- taddr )   DP-T @ ;
: ALLOT-T ( n -- )       HERE-T THERE OVER ERASE   DP-T +! ;
: C,-T    ( char -- )   HERE-T C!-T   1 DP-T +! ;
: W,-T    ( w -- )      HERE-T W!-T   2 DP-T +! ;
: ,-T     ( n -- )      HERE-T  !-T   CELL-T DP-T +! ;
: S,-T    ( addr len -- )
   0 ?DO   COUNT C,-T   LOOP   DROP ;

: ALIGN  BEGIN HERE-T CELL-T 1 - AND WHILE 0 C,-T REPEAT ;

: tdump  target-image here-t dump ;

\ Output to prims.inc
: ?ERR  ABORT" file I/O error" ;

VARIABLE OUT
: OPEN   R/W CREATE-FILE ?ERR OUT ! ;
: WRITE  ( adr len -- )  OUT @ WRITE-FILE ?ERR ;

S" prims.inc" OPEN

CREATE EOL 1 C, 0A C,
: NEWLINE   EOL COUNT WRITE ;

: `   1 PARSE WRITE  NEWLINE ;
: ``  BEGIN  REFILL 0= ABORT" missing ``"
        BL WORD COUNT S" ``" COMPARE WHILE
        SOURCE WRITE NEWLINE
      REPEAT ;

\ write decompiler information
variable seer
s" see.info" w/o create-file ?err seer !
: info ( opc -- )
    base @ decimal swap
    0 <# #S #> seer @ write-file ?err
    s"  OP " seer @ write-file ?err
    >in @  bl word count seer @ write-line ?err  >in !
    base ! ;

: CLOSE  OUT @ CLOSE-FILE ?ERR ;

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
    seer @ close-file ?err
    CLOSE
    CR ." Saving " BASE @ DECIMAL HERE-T . BASE ! ." bytes..."
    WRITE-DICT-IMG
    WRITE-DICT-INC
    ." done" ;

: ciao cr bye ;

\ **********************************************************************
\ Compiler

VARIABLE ?CODE
: LATEST ( -- n )  ?CODE @ DUP IF C@-T ELSE INVERT THEN ;
: PATCH  ( n -- )  ?CODE @ DUP 0= ABORT" patch?" C!-T ;
: OP,  ( opcode -- )  HERE-T ?CODE !  C,-T ;

: COMPILE,  ( addr -- )
    ?CODE @ 0< 0= IF
        DUP C@-T 5F >  OVER 1+ C@-T 0= AND IF  C@-T OP, EXIT  THEN
    THEN
    DUP 10000 U< IF  1 OP, W,-T ( call16 ) EXIT  THEN
    8 OP, ,-T ( call32 ) ;

: EXIT  0 OP, ;

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
    HERE-T LAST !  BL WORD COUNT DUP ( 80 OR)  C,-T S,-T  ALIGN ;

VARIABLE STATE-T
: ?EXEC  STATE-T @ 0= ABORT" cannot execute target word!" ;

VARIABLE CSP
: !CSP  DEPTH CSP ! ;
: ?CSP  DEPTH CSP @ - ABORT" definition not finished" ;

: TARGET-CREATE   ( -- )
   >IN @ HEADER >IN !  CREATE  HERE-T ,
   DOES>  ?EXEC  @ COMPILE, ;

\ Generate primatives
: ?COMMENT  ( allow Forth comment after OP: etc. )
    >IN @  BL WORD COUNT S" (" COMPARE
    IF  >IN !  ELSE  DROP  [COMPILE] (  THEN ;

: C-COMMENT  S" /* " WRITE  BL WORD COUNT WRITE  S"  */ " WRITE ;
VARIABLE OP  ( next opcode )
: OP!  OP ! ;
: OP:  ( output opcode case statement )
    OP @ FF > ABORT" opcodes exhausted"
    OP @ info
    C-COMMENT  S" case 0x" WRITE  OP @ 0 <# # # #> WRITE  S" : " WRITE
    ?COMMENT ` ( copy rest of line )  1 OP +! ;

: (PRIM)   OP @ OP,  EXIT  OP: ;
: PRIM   >IN @ HEADER        >IN ! (PRIM) ;  ( in target only )
: CODE   >IN @ TARGET-CREATE >IN ! (PRIM) ;  ( in host and target)

: BINARY  ( op -- )
    CREATE ,  DOES> ?EXEC @ OP, ;
\    CREATE ,  DOES> @  LATEST <LIT> =
\        IF  10 + PATCH  ELSE  OP,  THEN ;

\ Target Literals
: LITERAL  ( n -- )  ?EXEC  8 OP,  ,-T ;
: $   BL WORD NUMBER DROP LITERAL ;

\ Define Meta Branching Constructs
: ?CONDITION  INVERT ABORT" unbalanced" ;
: MARK      ( -- here )     ?EXEC  HERE-T  0 ?CODE ! ;
: >MARK     ( -- f addr )   TRUE  MARK   0 C,-T ;
: >RESOLVE  ( f addr -- )   MARK  OVER -  SWAP C!-T   ?CONDITION ;
: <MARK     ( -- f addr )   TRUE  MARK ;
: <RESOLVE  ( f addr -- )   MARK  - C,-T   ?CONDITION ;

: CONDITION  ( todo optimizer )
    58 OP, ;

: NOT  ( invert last conditional op )  ?EXEC  LATEST 70 78 WITHIN
    IF  LATEST 8 XOR PATCH  ELSE  70 OP, ( 0= )  THEN ;

: IF        CONDITION  >MARK ;
: THEN      >RESOLVE ;
: ELSE      3 OP,  >MARK  2SWAP >RESOLVE ;
: BEGIN     <MARK ;
: UNTIL     CONDITION  <RESOLVE ;
: AGAIN     3 OP,  <RESOLVE ;
: WHILE     IF  2SWAP ;
: REPEAT    AGAIN  THEN ;

: DO        4 OP,  >MARK  <MARK ;
: ?DO       5 OP,  >MARK  <MARK ;
: LOOP      6 OP,  <RESOLVE  >RESOLVE ;
: +LOOP     7 OP,  <RESOLVE  >RESOLVE ;

\ Compile Strings into the Target
: C"  HERE-T  [CHAR] " PARSE S,-T  0 C,-T ; \ c-style string
: ,"  ?EXEC  [CHAR] " PARSE  DUP C,-T  S,-T  0 ?CODE ! ;

: S"      0A OP,  ," ;
: ."      0B OP,  ," ;
: ABORT"  0C OP,  ," ;

\ Defining Words
: CONSTANT  TARGET-CREATE  10 OP, ,-T ;
: VARIABLE  TARGET-CREATE  11 OP, ALIGN 0 ,-T ;

: [   0 STATE-T ! ;
: ]  -1 STATE-T ! ;

: T:  HEADER   0 ?CODE !  ] ;  \ to create words with no host header

: ;_  [COMPILE] ; ; IMMEDIATE \ concession
: [COMPILE] ;
: ;   ?CSP EXIT [ ;
: :   TARGET-CREATE  0 ?CODE !  !CSP ] ;_


\ **********************************************************************
\ Start of Kernel
\ **********************************************************************

40 DP-T !

\ Variables shared with C code at fixed offsets
08 CONSTANT H
0C CONSTANT BASE
10 CONSTANT STATE
14 CONSTANT 'IN
18 CONSTANT CONTEXT ( 3 cells )

``
#define COLD M[0]
#define WARM M[1]
#define HERE M[2]
#define BASE M[3]
#define STATE M[4]
#define SOURCE M[5]
#define CONTEXT 6

cell *S, top;
cell *R;
byte *I;
cell w;

BASE = 10;

if (COLD) {
//  if (verbose) printf("Running from %u\n", COLD);
    I = abs(COLD);
    goto start;
}

abort:
    I = abs(WARM);
start:
    STATE = 0;
    M[CONTEXT] = 1;
    S = S0;
    R = R0;
next:
    // if (verbose > 2) printf("I=%X op=%02X R=%X %X %X (%d)\n",
    //     rel(I), *I, rel(R[0]), rel(R[1]), rel(R[2]), R0-R);
    switch (w = *I++) {

#define push *--S = top, top =
#define pop top = *S++
#define pop2 top = S[1], S += 2
#define pop3 top = S[2], S += 3
#define LOGICAL ? -1 : 0

#define NEXT        goto next;
#define LIT         *(cell*)I
#define OFFSET      *(signed char*)I
#define BRANCH      I += OFFSET
#define NOBRANCH    I += 1
#define EXIT        I = (byte*) *R++; NEXT
``

0 OP! ( special functions )

OP: EXIT        EXIT
OP: CALL        w = *(uint16_t *)I; *--R = (cell)I + 2; I = m + w; NEXT
OP: CALL32      w = LIT; *--R = (cell)I + CELL; I = m + w; NEXT
OP: BRANCH      BRANCH; NEXT
OP: DO          *--R = (cell)I + OFFSET, *--R = *S, *--R = top - *S++, pop; NOBRANCH; NEXT
OP: ?DO         if (top == *S) BRANCH;
                ` else *--R = (cell)I + OFFSET,
                `      *--R = *S, *--R = top - *S, NOBRANCH;
                ` S++, pop; NEXT
OP: LOOP        if ((++ *R) == 0) NOBRANCH, R += 3; else BRANCH; NEXT
OP: +LOOP       w = *R, *R += top;
                ` if ((w ^ *R) < 0 && (w ^ top) < 0) NOBRANCH, R += 3;
                ` else BRANCH; pop; NEXT

OP: LIT         push LIT; I += CELL; NEXT
OP: ---         NEXT
OP: S"          push rel(I) + 1; push *I; I = litq(I); NEXT
OP: ."          I = dotq(I); NEXT
OP: ABORT"      if (!top) { I = litq(I); pop; NEXT }
                ` show_error((char*)I, abs(HERE), abs(SOURCE));
                ` goto abort;
\ OP:
\ OP:
\ OP:

10 OP! ( runtime for defining words )

OP: DOCON       push LIT; EXIT
OP: DOVAR       push aligned(rel(I)); EXIT
OP: DOCREATE    push aligned(rel(I)) + CELL; w = *(cell*)aligned(I);
                ` if (w) I = abs(w); else EXIT

20 OP! ( lit op )

OP: LIT+        top += LIT, I += CELL; NEXT
OP: LIT-        top -= LIT, I += CELL; NEXT
OP: LIT*        top *= LIT, I += CELL; NEXT
OP: LIT/        top /= LIT, I += CELL; NEXT
OP: LITAND      top &= LIT, I += CELL; NEXT
OP: LITOR       top |= LIT, I += CELL; NEXT
OP: LITXOR      top ^= LIT, I += CELL; NEXT
OP: ---

OP: LIT@        push *(cell*)(m + LIT); I += CELL; NEXT
OP: LIT!        *(cell*)(m + LIT) = top, pop; I += CELL; NEXT

30 OP! ( lit cond : op | lit )
\ not needed for 0= 0< etc. so this frees up 6 slots, and be careful!
` #define LITCOND(cond) top = (cond) LOGICAL; I += CELL; NEXT

OP: ---
OP: ---
OP: ---
OP: LIT=        LITCOND(top == LIT)
OP: LIT<        LITCOND(top < LIT)
OP: LIT>        LITCOND(top > LIT)
OP: LITU<       LITCOND((ucell)top < (ucell)LIT)
OP: LITU>       LITCOND((ucell)top > (ucell)LIT)

OP: ---
OP: ---
OP: ---
OP: LIT<>       LITCOND(top != LIT)
OP: LIT>=       LITCOND(top >= LIT)
OP: LIT<=       LITCOND(top <= LIT)
OP: LITU>=      LITCOND((ucell)top >= (ucell)LIT)
OP: LITU<=      LITCOND((ucell)top <= (ucell)LIT)

40 OP! ( lit cond branch : op | lit | offset )
\ not needed for 0= 0< etc. so this frees up 6 slots, and be careful!

` #define LITIF(cond) w = LIT, I += CELL; if (cond) NOBRANCH; else BRANCH; pop; NEXT

OP: ---
OP: ---
OP: ---
OP: LIT=IF      LITIF(top == w)
OP: LIT<IF      LITIF(top < w)
OP: LIT>IF      LITIF(top > w)
OP: LITU<IF     LITIF((ucell)top < (ucell)w)
OP: LITU>IF     LITIF((ucell)top > (ucell)w)

OP: ---
OP: ---
OP: ---
OP: LIT<>IF     LITIF(top != w)
OP: LIT>=IF     LITIF(top >= w)
OP: LIT<=IF     LITIF(top <= w)
OP: LITU>=IF    LITIF((ucell)top >= (ucell)w)
OP: LITU<=IF    LITIF((ucell)top <= (ucell)w)

50 OP! ( cond branch )

` #define IF(cond)  if (cond) NOBRANCH; else BRANCH
` #define IF1(cond) IF(cond); pop; NEXT
` #define IF2(cond) IF(cond); pop2; NEXT

OP: 0=IF     IF1(top == 0)
OP: 0<IF     IF1(top < 0)
OP: 0>IF     IF1(top > 0)
OP: =IF      IF2(*S == top)
OP: <IF      IF2(*S < top)
OP: >IF      IF2(*S > top)
OP: U<IF     IF2((ucell)*S < (ucell)top)
OP: U>IF     IF2((ucell)*S > (ucell)top)

OP: ?BRANCH  IF1(top != 0)
OP: 0>=IF    IF1(top >= 0)
OP: 0<=IF    IF1(top <= 0)
OP: <>IF     IF2(*S != top)
OP: >=IF     IF2(*S >= top)
OP: <=IF     IF2(*S <= top)
OP: U>=IF    IF2((ucell)*S >= (ucell)top)
OP: U<=IF    IF2((ucell)*S <= (ucell)top)

60 OP! ( binary/memory ops )

60 BINARY +     61 BINARY -     62 BINARY *     63 BINARY /
64 BINARY AND   65 BINARY OR    66 BINARY XOR
\ 65 BINARY LSHIFT    66 BINARY RSHIFT

PRIM +          top += *S++; NEXT
PRIM -          top = *S++ - top; NEXT
PRIM *          top *= *S++; NEXT
PRIM /          top = *S++ / top; NEXT
PRIM AND        top &= *S++; NEXT
PRIM OR         top |= *S++; NEXT
PRIM XOR        top ^= *S++; NEXT
OP: ---

CODE @          top = *(cell *)(m + top); NEXT
CODE !          *(cell *)(m + top) = *S; pop2; NEXT


70 OP! ( conditionals )

CODE 0=         top = (top == 0) LOGICAL; NEXT
CODE 0<         top = (top < 0) LOGICAL; NEXT
CODE 0>         top = (top > 0) LOGICAL; NEXT
CODE =          top = (*S++ == top) LOGICAL; NEXT
CODE <          top = (*S++ < top) LOGICAL; NEXT
CODE >          top = (*S++ > top) LOGICAL; NEXT
CODE U<         top = ((ucell)*S++ < (ucell)top) LOGICAL; NEXT
CODE U>         top = ((ucell)*S++ > (ucell)top) LOGICAL; NEXT

OP: 0<>   top = (top != 0) LOGICAL; NEXT
OP: 0>=   top = (top >= 0) LOGICAL; NEXT
OP: 0<=   top = (top <= 0) LOGICAL; NEXT
OP: <>   top = (*S++ != top) LOGICAL; NEXT
OP: >=   top = (*S++ >= top) LOGICAL; NEXT
OP: <=   top = (*S++ <= top) LOGICAL; NEXT
OP: U>=   top = ((ucell)*S++ >= (ucell)top) LOGICAL; NEXT
OP: U<=   top = ((ucell)*S++ <= (ucell)top) LOGICAL; NEXT

80 OP! ( nothing special after this )

CODE DUP        *--S = top; NEXT
CODE DROP       pop; NEXT
CODE SWAP       w = top; top = *S; *S = w; NEXT
CODE OVER       push S[1]; NEXT
CODE ROT        w = S[1], S[1] = *S, *S = top, top = w; NEXT
CODE NIP        S++; NEXT
CODE ?DUP       if (top) *--S = top; NEXT
CODE PICK       top = S[top]; NEXT

CODE >R         *--R = top, pop; NEXT
CODE R>         push *R++; NEXT
CODE R@         push *R  ; NEXT

CODE I          push R[0] + R[1]; NEXT
CODE J          push R[3] + R[4]; NEXT
CODE LEAVE      I = (byte*)R[2];
CODE UNLOOP     R += 3; NEXT

: 2DUP      OVER OVER ;
: 2DROP     DROP DROP ;

CODE INVERT  top = ~top; NEXT
CODE NEGATE  top = -top; NEXT
CODE LSHIFT  top = *S++ << top; NEXT
CODE RSHIFT  top = ((ucell)*S++) >> top; NEXT

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

CODE +!         *(cell *)(m + top) += *S; pop2; NEXT

CODE C@  ( a -- c )  top = m[top]; NEXT
CODE C!  ( c a -- )  m[top] = *S; pop2; NEXT
: COUNT  DUP 1+ SWAP C@ ;

CODE 2@     *--S = *(cell*)(m + top + CELL); top = *(cell*)(m + top); NEXT
CODE 2!     *(cell*)(m + top) = *S++; *(cell*)(m + top + CELL) = *S++; pop; NEXT

( 16-bit fetch and store )
` #define W(a)  *(uint16_t *)(m + (a))
CODE W@     top = W(top); NEXT
CODE W!     W(top) = *S++, pop; NEXT

CODE FILL  ( a u c -- )       memset(abs(S[1]), top, *S);       pop3; NEXT
CODE MOVE  ( src dest u -- )  memmove(abs(*S), abs(S[1]), top); pop3; NEXT

CODE >ABS  top += (cell)m; NEXT   // convert to absolute address
CODE >REL  top -= (cell)m; NEXT   // convert to relative address

CODE KEY   ( -- char )  push getchar(); NEXT
CODE EMIT  ( char -- )  putchar(top); pop; NEXT
CODE TYPE  ( a n -- )   type(*S, top); pop2; NEXT

: CR     $ 0A EMIT ;
: SPACE  $ 20 EMIT ;

CODE COMPARE  top = compare(abs(S[2]), S[1], abs(*S), top); S += 3; NEXT
CODE SEARCH   top = search(S++, top); NEXT

CODE BYE  return 0;

CODE ACCEPT ( a n -- n )  top = accept(*S++, top);

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

: >IN           'IN @ ;
( source-len )
( source-addr )
: FILE          >IN $ 3 CELLS + ;
: 'TIB          >IN $ 4 CELLS + ;
: SOURCE-NAME   >IN $ 5 CELLS + ;
: SOURCE-LINE   >IN $ 6 CELLS + ;

: SOURCE        >IN CELL+ 2@ ;
: SOURCE-ID     FILE @ ;

: SOURCE-DEPTH  >IN SOURCE-STACK -  $ 5 RSHIFT ( 32 /) ;

CODE ALLOCATE   *--S = rel(malloc(top)), top = *S ? 0 : -1; NEXT
CODE RESIZE     *S = rel(realloc(abs(*S), top)), top = *S ? 0 : -1; NEXT
CODE FREE       if (top) free(abs(top)); top = 0; NEXT

CODE NEW-STRING top = new_string(*S++, top); NEXT

: FILE?  1+ $ 2 U< NOT ;

: >SOURCE ( filename len fileid | -1 -- ) \ CR ." Including " DROP TYPE SPACE ;
    SOURCE-DEPTH $ 7 U> ABORT" nested too deep"
    $ 20 'IN +!
    DUP FILE !
    FILE? IF  $ 80 ALLOCATE DROP 'TIB !  NEW-STRING SOURCE-NAME !  THEN
    $ 0 SOURCE-LINE ! ;

: SOURCE> ( -- )
    SOURCE-DEPTH $ 1 < ABORT" trying to pop empty source"
    SOURCE-ID FILE? IF
        SOURCE-ID CLOSE-FILE DROP
        'TIB @ FREE DROP
        SOURCE-NAME @ FREE DROP
    THEN
    $ -20 'IN +! ;

CODE REFILL ( -- f )  push refill(SOURCE); NEXT

VARIABLE TIB 80 ALLOT-T
: QUERY  ( -- )  $ 0 FILE !  TIB 'TIB !  REFILL 0= IF BYE THEN ;

\ ********** Numbers **********

CODE .  ( n -- )  printf("%d ", top); pop; NEXT

CODE -NUMBER  ( a -- a t, n f ) w = number(abs(top), --S, BASE);
`   if (w) top = 0; else *S = top, top = -1; NEXT
: NUMBER  ( a -- n )  -NUMBER ABORT" ? " ;

CODE >NUMBER  top = to_number(S, top, BASE); NEXT

20 CONSTANT BL

CODE WORD  ( char -- addr )
`   top = word(SOURCE, top, HERE); NEXT

CODE FIND  ( str -- xt flag | str 0 )
`       w = find(top, M[CONTEXT + 1]);
`       if (w) *--S = w, top = -1;
`       else push 0; NEXT

CODE -FIND  ( str v -- str t | xt f )
`       w = find(*S, M[CONTEXT + top]);
`       if (w) *S = w, top = 0;
`       else top = -1; NEXT

CODE >NAME ( xt -- nfa )  top = nfa(top); NEXT
CODE NAME> ( nfa -- xt )  top = cfa(top); NEXT

: -'  ( n - h t, a f )  $ 20 WORD SWAP -FIND ;
: '   ( -- a )   CONTEXT @ -' ABORT" ?" ;

CODE DEPTH ( -- n )  w = S0 - S; push w; NEXT
CODE .S ( -- )
`       w = S0 - S;  S[-1] = top;
`       printf("[%d] ", w);
`       for (int i = w - 2; i >= -1; i--)
`           printf("%d (0x%x) ", S[i], S[i]);
`       NEXT

CODE WORDS  ( -- )  words(M[CONTEXT + M[CONTEXT]]); NEXT
CODE DUMP  ( a n -- )  dump(*S++, top, BASE); pop; NEXT

( ********** Compiler ********** )

VARIABLE dA ( offset for target compiler )

: HERE   H @  ;
: ALLOT  H +! ;
: ,   H @ !  CELL H +! ;
: C,  H @ C!  $ 1 H +! ;
: W,  H @ W!  $ 2 H +! ;

\ : ,A  dA @ - , ;

CODE ALIGNED  top = aligned(top); NEXT
: ALIGN  BEGIN HERE DUP ALIGNED < WHILE $ 0 C, REPEAT ;

VARIABLE ?CODE 0 ,-T
: OP, ( opc -- )  ?CODE @ HERE ?CODE 2!  C, ;

COMPILER
: LITERAL  $ 8 OP, , ;
FORTH

: COMPILE,  ( xt -- )
\ TODO: multi-op inlining
    DUP C@ $ 5F > OVER 1+ C@ 0= AND IF  C@ OP,  EXIT THEN

    DUP C@ $ 10 = IF ( constant ) 1+ @              [COMPILE] LITERAL  EXIT THEN
    DUP C@ $ 11 = IF ( variable ) 1+ ALIGNED dA @ - [COMPILE] LITERAL  EXIT THEN

    DUP $ 10000 U< IF  $ 1 OP, dA @ - W,  EXIT THEN
    $ 8 OP, dA @ - , ;

( ********** Interpreter ********** )

CODE EXECUTE  *--R = (cell)I, I = m + top, pop; NEXT

: INTERPRET  ( -- )
    BEGIN   BL WORD DUP C@
    WHILE   STATE @
        IF    $ 2 -FIND IF  $ 1 -FIND IF  NUMBER  [COMPILE] LITERAL
              ELSE  COMPILE,  THEN  ELSE  EXECUTE  THEN
        ELSE  $ 1 -FIND IF  NUMBER  ELSE  EXECUTE  THEN
        THEN  DEPTH 0< ABORT" stack? "
    REPEAT DROP ;

CODE R0!  R = R0; NEXT

: QUIT [ HERE-T 4 !-T ] R0!
    BEGIN  SOURCE-DEPTH 0> WHILE  SOURCE>  REPEAT
    BEGIN  CR QUERY  INTERPRET  STATE @ 0= IF ."  ok" THEN  AGAIN ;

: INCLUDED  ( str len -- )
    2DUP R/O OPEN-FILE ABORT" file not found"
    >SOURCE  BEGIN REFILL WHILE INTERPRET REPEAT  SOURCE> ;

: INCLUDE  BL WORD COUNT INCLUDED ;

: BOOT  [ HERE-T 0 !-T ]
    SOURCE-STACK 'IN !
    ARGC $ 1 ?DO  I ARGV INCLUDED  LOOP
    ." Hello" QUIT ;

( ********** More compiler ********** )

CODE PARSE    ( c -- a n )  top = parse(SOURCE, top, --S); NEXT
CODE PARSE-NAME ( -- a n )  push parse_name(SOURCE, --S); NEXT

: S,  ( a n -- )  BEGIN DUP WHILE >R COUNT C, R> 1- REPEAT 2DROP ;

VARIABLE WARNING
: WARN  WARNING @ IF  >IN @  BL WORD CONTEXT @ -FIND 0= IF
    HERE COUNT TYPE ."  redefined " THEN  DROP >IN !  THEN ;

VARIABLE LAST ( link )
: CURRENT ( -- a )  CONTEXT @ CELLS CONTEXT + ;
: REVEAL  LAST @ CURRENT ! ;

: (HEADER)  ( -- )  WARN  $ 0 ?CODE !
\    ALIGN  HERE  DUP LAST !  CURRENT @ ,  CURRENT !
    ALIGN  HERE LAST !  CURRENT @ ,
    BL WORD C@  ( DUP $ 80 OR HERE C!)  1+ ALLOT  ALIGN ;
: HEADER  (HEADER) REVEAL ;

: CONSTANT  HEADER  $ 10 OP, , ;
: VARIABLE  HEADER  $ 11 OP, ALIGN $ 0 , ;

\ | opc | align | I for does | data
: CREATE  HEADER $ 12 C, ALIGN $ 0 , ;

: PREVIOUS  ( -- nfa count )  CURRENT @  CELL+ DUP C@ ;
\ : SMUDGE  LAST @ IF  PREVIOUS  $ 20 XOR  SWAP C!  THEN ;
: DOES>   R> >REL  PREVIOUS $ 1F AND + 1+ ALIGNED ( cfa ) 1+ ALIGNED ! ;

\ Be careful from here on...

COMPILER
: [  $ 0 STATE ! ;
: EXIT  $ 0 OP, ;
T: ;  EXIT [ REVEAL ;
: [COMPILE]  $ 2 -' ABORT" ?" COMPILE, ;
FORTH

: ]  $ -1 STATE ! ;
T: :  (HEADER) ] ;

HERE-T 8 !-T  ( here )
18 EMPLACE  ( context )
SAVE
