\ McForth Kernel
\
\ Copyright (c) 2020 Andrew McKewan
\
\ Permission is hereby granted, free of charge, to any person obtaining a copy
\ of this software and associated documentation files (the "Software"), to deal
\ in the Software without restriction, including without limitation the rights
\ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
\ copies of the Software, and to permit persons to whom the Software is
\ furnished to do so, subject to the following conditions:
\
\ The above copyright notice and this permission notice shall be included in all
\ copies or substantial portions of the Software.
\
\ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
\ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
\ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
\ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
\ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
\ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
\ SOFTWARE.


\ Variables shared with C code at fixed offsets

( COLD )  0 ,  ( WARM ) 0 ,  ( H ) 0 ,  ( BASE ) #10 ,
( STATE ) 0 ,  ( 'IN )  0 ,
( NULL ) 0 , 0 , 8009 , ( NOP R>DROP EXIT )
( CONTEXT ) 1 ,  0 , 0 ,

2 +ORIGIN CONSTANT H
3 +ORIGIN CONSTANT BASE
\ 4 +ORIGIN CONSTANT STATE
5 +ORIGIN CONSTANT 'IN
9 +ORIGIN CONSTANT CONTEXT

``
#define COLD M[0]
#define WARM M[1]
#define HERE M[2]
#define BASE M[3]
//#define STATE M[4]
#define SOURCE M[5]
#define CONTEXT 9

byte *I;
cell *S, top;
cell *R;
cell w;

I = abs(COLD);
goto start;

abort:
    show_error((char*)I, abs(HERE), abs(SOURCE));
    I = abs(WARM);
start:
//    STATE = 0;
    M[CONTEXT] = 1;
    S = S0;
    R = R0;
next:
#if 0
    if (verbose > -1) {
        printf("I=%tX op=%02X ", rel(I), *I);
        printf("R=%tX %tX %tX (%td) ", R[0], R[1], R[2], R0-R);
        printf("S=[%td] %tX %tX %tX %tX ", S0-S, S[2], S[1], S[0], top);
        printf("H=%tX\n", HERE);
    }
#endif

    switch (*I++) {

#define push *--S = top, top =
#define pop top = *S++
#define pop2 top = S[1], S += 2
#define pop3 top = S[2], S += 3
#define LOGICAL ? -1 : 0

#define NEXT        goto next;
#define LIT         at(I)
#define OFFSET      *(int8_t *)I
#define BRANCH      I += OFFSET
#define NOBRANCH    I += 1
#define EXIT        I = (byte *)*R++; NEXT
`

0 OP! ( special functions )

OP: EXIT        EXIT
OP: CALL        w = *(uint16_t *)I; *--R = (cell)I + 2; I = m + CELLS(w); NEXT
OP: CALLX       w = LIT; *--R = (cell)I + CELL; I = m + w; NEXT
OP: BRANCH      BRANCH; NEXT
OP: ?DO         if (top == *S) { pop2, BRANCH; NEXT }
OP: DO          *--R = (cell)I + OFFSET, *--R = *S, *--R = top - *S++, pop;
                ` NOBRANCH; NEXT
OP: LOOP        if ((++ *R) == 0) NOBRANCH, R += 3; else BRANCH; NEXT
OP: +LOOP       w = *R, *R += top;
                ` if ((w ^ *R) < 0 && (w ^ top) < 0) NOBRANCH, R += 3;
                ` else BRANCH; pop; NEXT

OP: JUMP        w = *(uint16_t *)I; I = m + CELLS(w); NEXT
OP: NOP         NEXT
OP: S"          w = *I++, push rel(I), push w, I += w; NEXT
OP: ."          I = dotq(I); NEXT
OP: ABORT"      if (top) goto abort; w = *I++, I += w, pop; NEXT

10 OP! ( runtime for defining words )

OP: DOCON       push at(aligned(I)); EXIT
OP: DOVAR       push rel(aligned(I)); EXIT
OP: DODOES      push rel(aligned(I)); w = at(I - 1) >> 8;
                ` if (w) I = abs(w); else EXIT
OP: DOVALUE     push at(aligned(I)); EXIT
OP: DODEFER     w = at(aligned(I)); I = m + w; NEXT

20 OP! ( lit op )

OP: LIT         push LIT,   I += CELL; NEXT
OP: LIT+        top += LIT, I += CELL; NEXT
OP: LIT-        top -= LIT, I += CELL; NEXT
OP: LIT*        top *= LIT, I += CELL; NEXT
OP: LIT/        top /= LIT, I += CELL; NEXT
OP: LITAND      top &= LIT, I += CELL; NEXT
OP: LITOR       top |= LIT, I += CELL; NEXT
OP: LITXOR      top ^= LIT, I += CELL; NEXT

OP: LIT@        push AT(LIT); I += CELL; NEXT
OP: LIT!        AT(LIT)  = top, pop; I += CELL; NEXT
OP: LIT+!       AT(LIT) += top, pop; I += CELL; NEXT

30 OP! ( lit cond : op | lit )

` #define LITCOND(cond) top = (cond) LOGICAL; I += CELL; NEXT

---
---
---
OP: LIT=        LITCOND(top == LIT)
OP: LIT<        LITCOND(top < LIT)
OP: LIT>        LITCOND(top > LIT)
OP: LITU<       LITCOND((ucell)top < (ucell)LIT)
OP: LITU>       LITCOND((ucell)top > (ucell)LIT)

---
---
---
OP: LIT<>       LITCOND(top != LIT)
OP: LIT>=       LITCOND(top >= LIT)
OP: LIT<=       LITCOND(top <= LIT)
OP: LITU>=      LITCOND((ucell)top >= (ucell)LIT)
OP: LITU<=      LITCOND((ucell)top <= (ucell)LIT)

40 OP! ( lit cond branch : op | lit | offset )

` #define LITIF(cond) w = LIT, I += CELL; if (cond) NOBRANCH; else BRANCH; pop; NEXT

---
---
---
OP: LIT=IF      LITIF(top == w)
OP: LIT<IF      LITIF(top < w)
OP: LIT>IF      LITIF(top > w)
OP: LITU<IF     LITIF((ucell)top < (ucell)w)
OP: LITU>IF     LITIF((ucell)top > (ucell)w)

---
---
---
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

OP: 0<>IF    IF1(top != 0)
OP: 0>=IF    IF1(top >= 0)
OP: 0<=IF    IF1(top <= 0)
OP: <>IF     IF2(*S != top)
OP: >=IF     IF2(*S >= top)
OP: <=IF     IF2(*S <= top)
OP: U>=IF    IF2((ucell)*S >= (ucell)top)
OP: U<=IF    IF2((ucell)*S <= (ucell)top)

60 OP! ( binary/memory ops )

---
CODE +          top = *S++ + top; NEXT
CODE -          top = *S++ - top; NEXT
CODE *          top = *S++ * top; NEXT
CODE /          top = *S++ / top; NEXT
CODE AND        top = *S++ & top; NEXT
CODE OR         top = *S++ | top; NEXT
CODE XOR        top = *S++ ^ top; NEXT

CODE @          top = *(cell *)(m + top); NEXT
CODE !          *(cell *)(m + top) = *S; pop2; NEXT
CODE +!         *(cell *)(m + top) += *S; pop2; NEXT

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

80 OP! ( nothing special after this, except for R>DROP )

CODE R>DROP     ++R; NEXT // don't move
CODE DUP>R      *--R = top; NEXT

CODE >R         *--R = top, pop; NEXT
CODE R>         push *R++; NEXT
CODE R@         push *R  ; NEXT

CODE DUP        *--S = top; NEXT
CODE DROP       pop; NEXT
CODE SWAP       w = top; top = *S; *S = w; NEXT
CODE OVER       push S[1]; NEXT
CODE ROT        w = S[1], S[1] = *S, *S = top, top = w; NEXT
CODE NIP        S++; NEXT
CODE ?DUP       if (top) *--S = top; NEXT
CODE PICK       top = S[top]; NEXT

CODE I          push R[0] + R[1]; NEXT
CODE J          push R[3] + R[4]; NEXT
CODE LEAVE      I = (byte*)R[2];
CODE UNLOOP     R += 3; NEXT

CODE 2DUP   w = *S, *--S = top, *--S = w; NEXT
CODE 2DROP  top = S[1], S += 2; NEXT
CODE 2SWAP  w = S[0], S[0] = S[2], S[2] = w, w = S[1], S[1] = top, top = w; NEXT
CODE 2OVER  w = S[2], *--S = top, *--S = w, top = S[3]; NEXT

CODE INVERT  top = ~top; NEXT
CODE NEGATE  top = -top; NEXT
CODE LSHIFT  top = *S++ << top; NEXT
CODE RSHIFT  top = ((ucell)*S++) >> top; NEXT

` #define LOWER(u1,u2)  ((ucell)(u1) < (ucell)(u2))
CODE WITHIN  w = *S++, top = LOWER(*S - w, top - w) LOGICAL; S++; NEXT

CODE MOD   top = *S++ % top;  NEXT
CODE UMOD  top = (ucell)*S++ % (ucell)top;  NEXT

CODE M*  ( n1 n2 -- d ) {
`   int64_t d = (int64_t)*S * (int64_t)top;
`   *S = d ;
`   top = d >> 32;
`   NEXT }

CODE UM* ( u1 u2 -- ud ) {
`   uint64_t u1 = (ucell)*S;
`   uint64_t u2 = (ucell)top;
`   uint64_t ud = u1 * u2;
`   *S = ud ;
`   top = ud >> 32;
`   NEXT }

CODE UM/MOD  ( ud u1 -- rem quot ) {
`   uint64_t ud = ((uint64_t)*S << 32) | (ucell)S[1];
`   uint64_t u = (ucell)top;
`   ucell quot = ud / u;
`   ucell rem = ud % u;
`   *++S = rem;
`   top = quot;
`   NEXT }

CODE SM/REM  ( d n -- rem quot ) {
`   int64_t d = (((uint64_t)*S) << 32) | ((ucell) S[1]);
`   int32_t quot = d / top;
`   int32_t rem = d % top;
`   *++S = rem;
`   top = quot;
`   NEXT }

: 1+  $ 1 + ;
: 1-  $ 1 - ;
CODE 2*  top <<= 1; NEXT
CODE 2/  top >>= 1; NEXT

CELL CONSTANT CELL
: CELL+  CELL + ;

CODE COUNT  *--S = top + 1;
CODE C@     top = m[top]; NEXT
CODE C!     m[top] = *S; pop2; NEXT

CODE 2@     *--S = AT(top + CELL); top = AT(top); NEXT
CODE 2!     AT(top) = *S++; AT(top + CELL) = *S++; pop; NEXT

( 16-bit fetch and store )
` #define W(a)  *(uint16_t *)(m + (a))
CODE W@     top = W(top); NEXT
CODE W!     W(top) = *S++, pop; NEXT

CODE FILL  ( a u c -- )       memset(abs(S[1]), top, *S);       pop3; NEXT
CODE MOVE  ( src dest u -- )  memmove(abs(*S), abs(S[1]), top); pop3; NEXT

CODE M  push (cell)m; NEXT

CODE KEY   ( -- char )  push getchar(); NEXT
CODE EMIT  ( char -- )  putchar(top); pop; NEXT
CODE TYPE  ( a n -- )   type(*S, top); pop2; NEXT

: CR     $ 0A EMIT ;
: SPACE  $ 20 EMIT ;

CODE COMPARE  top = compare(abs(S[2]), S[1], abs(*S), top); S += 3; NEXT
CODE SEARCH   top = search(S++, top); NEXT

CODE BYE  return 0;

CODE ACCEPT ( a n -- n )  top = accept(*S++, top); NEXT

CODE ARGC ( -- n ) push argc; NEXT
CODE ARGV ( n -- a n ) *--S = rel(argv[top]); top = (cell)strlen(argv[top]); NEXT

CODE GETENV  ( name len -- value len )  top = get_env(S, top); NEXT
CODE SETENV  ( value len name len -- )  set_env(S, top); S += 3, pop; NEXT

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

\ Memory allocation
CODE ALLOCATE ( n -- a ior )    *--S = rel(malloc(top)), top = *S ? 0 : -1; NEXT
CODE RESIZE   ( a n -- a' ior ) *S = rel(realloc(abs(*S), top)), top = *S ? 0 : -1; NEXT
CODE FREE     ( a -- ior )      if (top) free(abs(top)); top = 0; NEXT

\ Allocate counted and null-terminate string
CODE NEW-STRING ( adr len -- c-str ) top = rel(new_string(abs(*S++), top)); NEXT

( ********** Input source processig ********** )

\ 8 entries * 8 cells per entry
40 CELLS BUFFER SOURCE-STACK

: CELLS  CELL * ;

: >IN   'IN @ ;
: FILE  'IN @ $ 4 CELLS + ;
: #TIB  'IN @ $ 1 CELLS + ;
: 'TIB  'IN @ $ 2 CELLS + ;

: SOURCE-BUF    >IN $ 2 CELLS + ;
: SOURCE-FILE   >IN $ 3 CELLS + ;
: SOURCE-NAME   >IN $ 4 CELLS + ;
\ : SOURCE-LINE   >IN $ 5 CELLS + ;

CODE SOURCE-POS ( -- col line )
    ` *--S = top, top = source_position(abs(SOURCE), --S); NEXT

: SOURCE        >IN CELL+ 2@ ;
: SOURCE-ID     SOURCE-FILE @ ;

: SOURCE-DEPTH  >IN SOURCE-STACK -  $ 5 RSHIFT ( 32 /) ;

: FILE? ( source-id -- f )  1+ $ 2 U< NOT ;

: >SOURCE ( adr len filename -- )
    SOURCE-DEPTH $ 7 U> ABORT" nested too deep"
    $ 8 CELLS 'IN +!
( concession ) $ -1 SOURCE-FILE !
    FILE !  #TIB 2!  $ 0 >IN ! ;

: SOURCE> ( -- )
    SOURCE-DEPTH $ 1 < ABORT" trying to pop empty source"
    FILE @ IF  FILE @ FREE DROP  'TIB @ FREE DROP  THEN
    $ -8 CELLS 'IN +! ;

CODE REFILL ( -- f )  push refill(SOURCE); NEXT

80 BUFFER TIB
: QUERY  $ 0 SOURCE-FILE !  TIB SOURCE-BUF !  REFILL 0= IF BYE THEN ;

\ ********** Numbers **********

` #define dot(n)  printf(BASE == 16 ? "%tx " : "%td ", n)

CODE .  ( n -- )  dot(top), pop; NEXT

CODE -NUMBER  ( a -- a t, n f ) w = number(abs(top), --S, BASE);
`   if (w) top = 0; else *S = top, top = -1; NEXT
: NUMBER  ( a -- n )  -NUMBER ABORT" ? " ;

CODE >NUMBER  top = to_number(S, top, BASE); NEXT

20 CONSTANT BL

CODE PARSE    ( c -- a n )  top = parse(SOURCE, top, --S); NEXT
CODE PARSE-NAME ( -- a n )  push parse_name(SOURCE, --S); NEXT

CODE WORD  ( char -- addr )
`   top = word(SOURCE, top, HERE); NEXT

CODE -FIND  ( str v -- str t | xt f )
`       w = find(*S, M[CONTEXT + top]); // search v
`       if (w) *S = w < 0 ? -w : w, top = 0;
`       else top = -1; NEXT

: -'  ( n - h t, a f )  $ 20 WORD SWAP -FIND ;
: '   ( -- a )   CONTEXT @ -' ABORT" ?" ;

CODE >NAME ( xt -- nfa )  top = xt_to_name(top); NEXT
CODE NAME> ( nfa -- xt )  top = name_to_xt(top); NEXT

CODE DEPTH ( -- n )  w = S0 - S; push w; NEXT
CODE .S ( -- )
`       w = S0 - S; if (w <= 0) { printf("empty "); NEXT }
`       // printf("[%td] ", w);
`       S[-1] = top;
`       for (w -= 2; w >= -1; w--) dot(S[w]);
`       NEXT

CODE WORDS  ( -- )  words(M[CONTEXT + M[CONTEXT]]); NEXT
CODE DUMP  ( a n -- )  dump(*S++, top, BASE); pop; NEXT

( ********** Compiler ********** )

VARIABLE dA ( offset for target compiler )
VARIABLE ?CODE 0 ,

: -OPT  $ 0 ?CODE ! ;

: HERE   H @  ;
: ALLOT  H +! ;
: ,   H @ !  CELL H +! ;
: C,  H @ C!  $ 1 H +! ;
: W,  H @ W!  $ 2 H +! ;

CODE ALIGNED  top = aligned(top); NEXT
: ALIGN  BEGIN HERE DUP ALIGNED < WHILE $ 0 C, REPEAT ;

: OP, ( opc -- )  ?CODE @ HERE ?CODE 2!  C, ;

COMPILER
: LITERAL  $ 20 OP, , ;
FORTH

\ Inline just primatives >= $60
: INLINE?  ( xt -- n t | f )
    DUP BEGIN  DUP C@ WHILE
        COUNT $ 60 < IF  2DROP $ 0 EXIT  THEN
    REPEAT SWAP - $ -1 ;

: INLINE ( xt n -- )
    OVER + SWAP BEGIN  2DUP U> WHILE  COUNT OP, REPEAT  2DROP ;

: COMPILE,  ( xt -- )
    DUP INLINE? IF INLINE EXIT THEN

    DUP C@ $ 10 = IF ( constant ) CELL+ @      \\ LITERAL  EXIT THEN
    DUP C@ $ 11 = IF ( variable ) CELL+ dA @ - \\ LITERAL  EXIT THEN
    DUP C@ $ 13 = IF ( value )    CELL+ dA @ - $ 28 OP, ,  EXIT THEN

    \ inline lit op exit (e.g. HERE, >IN)
    \ Not worth it, only a few words get optimized
    \ DUP COUNT $ E0 AND $ 20 = SWAP CELL+ C@ 0= AND IF  COUNT OP, @ , EXIT  THEN

\ No need yet to support far calls (> 256K dictionary)
\    DUP $ 10000 CELLS U< NOT IF  $ 2 OP, dA @ - ,  EXIT THEN

\ Optional check for bad behavior!
\    DUP CELL 1- AND ABORT" xt not aligned"
    $ 1 OP, dA @ - CELL / W, ;

CODE EXECUTE  *--R = (cell)I, I = m + top, pop; NEXT

: ]  BEGIN  $ 2 -' IF  $ 1 -FIND IF  NUMBER \\ LITERAL  ELSE  COMPILE,  THEN
        ELSE  EXECUTE  THEN AGAIN ;

( ********** Interpreter ********** )

: ?STACK  DEPTH 0< ABORT" stack?" ;

: INTERPRET  BEGIN  $ 1 -' IF  NUMBER  ELSE  EXECUTE ?STACK  THEN  AGAIN ;

CODE RESET  R = R0; NEXT

: QUIT  RESET
    BEGIN  SOURCE-DEPTH WHILE  SOURCE>  REPEAT
    BEGIN  CR QUERY  INTERPRET ."  ok"  AGAIN ;
1 HAS QUIT

( read whole file into memory )
CODE READ  ( name len -- addr len ior )  push(readall(S)); NEXT

: INCLUDED  ( str len -- )
    2DUP READ ABORT" can't read file"  2SWAP NEW-STRING
    >SOURCE  INTERPRET  SOURCE> ;

: INCLUDE  PARSE-NAME INCLUDED ;

TAG TAG

: COLD
    SOURCE-STACK 'IN !
    ARGC $ 1 ?DO  I ARGV INCLUDED  LOOP
    TAG COUNT TYPE  QUIT ;
0 HAS COLD


( ********** Defining Words ********** )

VARIABLE WARNING
: WARN  WARNING @ IF  >IN @  BL WORD CONTEXT @ -FIND 0= IF
    HERE COUNT TYPE ."  redefined " THEN  DROP >IN !  THEN ;

: LAST ( -- link )  CONTEXT @ CELLS  CONTEXT + ;
: PREVIOUS ( -- nfa count )  LAST @ CELL+  DUP C@ ;

: SMUDGE     PREVIOUS  $ 20 OR   SWAP C! ;
\ : REVEAL     PREVIOUS  $ DF AND  SWAP C! ;
COMPILER
: RECURSIVE  PREVIOUS  $ DF AND  SWAP C! ;
FORTH

\ VARIABLE 'RECURSE ( or just don't use it in :noname, duh )

: LINK,   ALIGN HERE  OVER @ ,  SWAP ! ;
: HEADER  WARN  LAST LINK,  BL WORD C@ 1+ ALLOT  ALIGN  -OPT ;

: CONSTANT  HEADER  $ 10 , , ;
: CREATE    HEADER  $ 11 , ;
: VARIABLE  CREATE $ 0 , ;

\ | opc | I for does | data
: DOES>   R> M -  dA @ -  $ 8 LSHIFT $ 12 OR
          PREVIOUS $ 1F AND + 1+ ALIGNED ( cfa ) ! ;
: >BODY   CELL+ ;

\ Be careful from here on...

COMPILER
: [  R>DROP ;
: EXIT  $ 0 OP, ;
T: ;  \\ EXIT  R>DROP  \\ RECURSIVE ; forget
: \\  $ 2 -' ABORT" ?" COMPILE, ;
\ : RECURSE  'RECURSE @ COMPILE, ;
FORTH
T: :  HEADER SMUDGE  ] ;
