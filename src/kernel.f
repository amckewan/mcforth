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
( CONTEXT ) 1 ,  0 ,  HERE 0 , 2001 ,  HERE SWAP ,A 2001 ,  ,A

2 +ORIGIN CONSTANT H
3 +ORIGIN CONSTANT BASE
4 +ORIGIN CONSTANT STATE
5 +ORIGIN CONSTANT 'IN
9 +ORIGIN CONSTANT CONTEXT

``
#define COLD M[0]
#define WARM M[1]
#define HERE M[2]
#define BASE M[3]
#define STATE M[4]
#define SOURCE M[5]
#define CONTEXT 9

cell *S, top;
cell *R;
byte *I;
cell w;

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
#if 0
    if (verbose > -1) {
        printf("I=%tX op=%02X ", rel(I), *I);
        printf("R=%tX %tX %tX (%td) ", R[0], R[1], R[2], R0-R);
        printf("S=[%td] %tX %tX %tX %tX ", S0-S, S[2], S[1], S[0], top);
        printf("H=%tX\n", HERE);
    }
#endif

    switch (w = *I++) {

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
OP: NOP         NEXT
OP: S"          w = *I++, push rel(I), push w, I += w; NEXT
OP: ."          I = dotq(I); NEXT
OP: ABORT"      if (!top) { w = *I++, I += w, pop; NEXT }
                ` show_error((char*)I, abs(HERE), abs(SOURCE));
                ` goto abort;
OP: ---
OP: ---
OP: ---

10 OP! ( runtime for defining words )

OP: DOCON       push at(aligned(I)); EXIT
OP: DOVAR       push rel(aligned(I)); EXIT
OP: DOCREATE    push rel(aligned(I)); w = at(I - 1) >> 8;
                ` if (w) I = abs(w); else EXIT
OP: DOVALUE     push at(aligned(I)); EXIT
OP: DODEFER     w = at(aligned(I)); I = m + w; NEXT


20 OP! ( lit op )
` #define LITOP(op) top op LIT, I += CELL; NEXT

OP: LIT+        LITOP(+=)
OP: LIT-        top -= LIT, I += CELL; NEXT
OP: LIT*        top *= LIT, I += CELL; NEXT
OP: LIT/        top /= LIT, I += CELL; NEXT
OP: LITAND      top &= LIT, I += CELL; NEXT
OP: LITOR       top |= LIT, I += CELL; NEXT
OP: LITXOR      top ^= LIT, I += CELL; NEXT
OP: ---

OP: LIT@        push AT(LIT); I += CELL; NEXT
OP: LIT!        AT(LIT)  = top, pop; I += CELL; NEXT
OP: LIT+!       AT(LIT) += top, pop; I += CELL; NEXT

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

CODE +          top += *S++; NEXT
CODE -          top = *S++ - top; NEXT
CODE *          top *= *S++; NEXT
CODE /          top = *S++ / top; NEXT
CODE AND        top &= *S++; NEXT
CODE OR         top |= *S++; NEXT
CODE XOR        top ^= *S++; NEXT
OP: ---

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

: 2DUP      OVER OVER ;
: 2DROP     DROP DROP ;

CODE INVERT  top = ~top; NEXT
CODE NEGATE  top = -top; NEXT
CODE LSHIFT  top = *S++ << top; NEXT
CODE RSHIFT  top = ((ucell)*S++) >> top; NEXT

CODE MOD   top = *S++ % top;  NEXT
CODE UMOD  top = (ucell)*S++ % (ucell)top;  NEXT

` #define LOWER(u1,u2)  ((ucell)(u1) < (ucell)(u2))

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

CODE 1+     top += 1; NEXT
CODE 1-     top -= 1; NEXT
CODE 2*     top <<= 1; NEXT
CODE 2/     top >>= 1; NEXT

CELL CONSTANT CELL
: CELL+  CELL + ;

CODE C@  ( a -- c )  top = m[top]; NEXT
CODE C!  ( c a -- )  m[top] = *S; pop2; NEXT
: COUNT  DUP 1+ SWAP C@ ;

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

\ 8 entries * 8 cells per entry
40 CELLS BUFFER SOURCE-STACK

CODE CELLS      top *= CELL; NEXT

: >IN           'IN @ ;
: SOURCE-BUF    >IN $ 2 CELLS + ;
: SOURCE-FILE   >IN $ 3 CELLS + ;
: SOURCE-NAME   >IN $ 4 CELLS + ;
: SOURCE-LINE   >IN $ 5 CELLS + ;

: SOURCE        >IN CELL+ 2@ ;
: SOURCE-ID     SOURCE-FILE @ ;

: SOURCE-DEPTH  >IN SOURCE-STACK -  $ 5 RSHIFT ( 32 /) ;

CODE ALLOCATE   *--S = rel(malloc(top)), top = *S ? 0 : -1; NEXT
CODE RESIZE     *S = rel(realloc(abs(*S), top)), top = *S ? 0 : -1; NEXT
CODE FREE       if (top) free(abs(top)); top = 0; NEXT

CODE NEW-STRING top = rel(new_string(abs(*S++), top)); NEXT

: FILE?  1+ $ 2 U< NOT ;

: >SOURCE ( filename fileid | -1 -- ) \ CR ." Including " DROP TYPE SPACE ;
    SOURCE-DEPTH $ 7 U> ABORT" nested too deep"
    $ 8 CELLS 'IN +!
    DUP SOURCE-FILE !
    FILE? IF  $ 80 ALLOCATE DROP SOURCE-BUF !  SOURCE-NAME !  THEN
    $ 0 SOURCE-LINE ! ;

: SOURCE> ( -- )
    SOURCE-DEPTH $ 1 < ABORT" trying to pop empty source"
    SOURCE-ID FILE? IF
        SOURCE-ID CLOSE-FILE DROP
        SOURCE-BUF @ FREE DROP
        SOURCE-NAME @ FREE DROP
    THEN
    $ -8 CELLS 'IN +! ;

CODE REFILL ( -- f )  push refill(SOURCE); NEXT

80 BUFFER TIB
: QUERY  $ 0 SOURCE-FILE !  TIB SOURCE-BUF !  REFILL 0= IF BYE THEN ;

\ ********** Numbers **********

CODE .  ( n -- )  printf("%td ", top); pop; NEXT

CODE -NUMBER  ( a -- a t, n f ) w = number(abs(top), --S, BASE);
`   if (w) top = 0; else *S = top, top = -1; NEXT
: NUMBER  ( a -- n )  -NUMBER ABORT" ? " ;

CODE >NUMBER  top = to_number(S, top, BASE); NEXT

20 CONSTANT BL

CODE PARSE    ( c -- a n )  top = parse(SOURCE, top, --S); NEXT
CODE PARSE-NAME ( -- a n )  push parse_name(SOURCE, --S); NEXT

CODE WORD  ( char -- addr )
`   top = word(SOURCE, top, HERE); NEXT

CODE FIND  ( str -- xt flag | str 0 )
`       w = find(top, M[CONTEXT + 1]); // search FORTH only
`       if (w > 0) *--S = w, top = -1;
`       else if (w < 0) *--S = -w, top = 1;
`       else push 0; NEXT

CODE -FIND  ( str v -- str t | xt f )
`       w = find(*S, M[CONTEXT + top]); // search v
`       if (w) *S = w < 0 ? -w : w, top = 0;
`       else top = -1; NEXT

CODE >NAME ( xt -- nfa )  top = xt_to_name(top); NEXT
CODE NAME> ( nfa -- xt )  top = name_to_xt(top); NEXT

: -'  ( n - h t, a f )  $ 20 WORD SWAP -FIND ;
: '   ( -- a )   CONTEXT @ -' ABORT" ?" ;

CODE DEPTH ( -- n )  w = S0 - S; push w; NEXT
CODE .S ( -- )
`       w = S0 - S;  S[-1] = top;
`       printf("[%td] ", w);
`       for (int i = w - 2; i >= -1; i--)
`           printf("%td (0x%tx) ", S[i], S[i]);
`       NEXT

CODE WORDS  ( -- )  words(M[CONTEXT + M[CONTEXT]]); NEXT
CODE DUMP  ( a n -- )  dump(*S++, top, BASE); pop; NEXT
CODE VERBOSE  push (byte *)&verbose - m; NEXT

( ********** Compiler ********** )

VARIABLE dA ( offset for target compiler )
VARIABLE ?CODE 0 ,

: -OPT  $ 0 ?CODE ! ;

: HERE   H @  ;
: ALLOT  H +! ;
: ,   H @ !  CELL H +! ;
: C,  H @ C!  $ 1 H +! ;
: W,  H @ W!  $ 2 H +! ;

\ : ,A  dA @ - , ;

CODE ALIGNED  top = aligned(top); NEXT
: ALIGN  BEGIN HERE DUP ALIGNED < WHILE $ 0 C, REPEAT ;

: OP, ( opc -- )  ?CODE @ HERE ?CODE 2!  C, ;

COMPILER
: LITERAL  $ 8 OP, , ;
FORTH

: INLINE?  ( xt -- n t | f ) \ count inlineable ops
    $ 0 SWAP BEGIN  COUNT DUP WHILE
        $ 60 < IF  2DROP $ 0 EXIT  THEN
        SWAP 1+ SWAP
    REPEAT DROP ;

: INLINE ( xt n -- )
    BEGIN  DUP WHILE  SWAP COUNT OP,  SWAP 1- REPEAT  2DROP ;

: COMPILE,  ( xt -- )
    DUP INLINE? IF INLINE EXIT THEN

    DUP C@ $ 10 = IF ( constant ) CELL+ @      \\ LITERAL  EXIT THEN
    DUP C@ $ 11 = IF ( variable ) CELL+ dA @ - \\ LITERAL  EXIT THEN
    DUP C@ $ 13 = IF ( value )    CELL+ dA @ - $ 28 OP, ,  EXIT THEN

    \ inline lit op exit (e.g. : 1+ 1 + ;)
    DUP COUNT $ 20 $ 30 WITHIN SWAP CELL+ C@ 0= AND IF  COUNT OP, @ , EXIT  THEN

    DUP $ 10000 U< IF  $ 1 OP, dA @ - W,  EXIT THEN
    $ 8 OP, dA @ - , ;

( ********** Interpreter ********** )

CODE EXECUTE  *--R = (cell)I, I = m + top, pop; NEXT

: ?STACK  DEPTH 0< ABORT" stack?" ;

: INTERPRET  ( -- )
    BEGIN  STATE @
        IF  $ 6 -'
            IF  FIND DUP
                IF  0< IF  COMPILE,  ELSE  EXECUTE  THEN
                ELSE  DROP NUMBER \\ LITERAL
                THEN
            ELSE  EXECUTE
            THEN
        ELSE  $ 1 -' IF  NUMBER  ELSE  EXECUTE ?STACK  THEN
        THEN
    AGAIN ;

CODE R0!  R = R0; NEXT

: QUIT  R0!
    BEGIN  SOURCE-DEPTH 0> WHILE  SOURCE>  REPEAT
    BEGIN  CR QUERY  INTERPRET  STATE @ 0= IF ."  ok" THEN  AGAIN ;
1 HAS QUIT

CODE OPEN-ON-PATH  ( str len -- filename fid ior )
    ` top = open_on_path(S, top, SOURCE);
    ` *--S = top, top = top ? 0 : -1; NEXT

: INCLUDED  ( str len -- )
    2DUP OPEN-ON-PATH IF  2DROP CR TYPE  $ -1 ABORT" file not found" THEN
    >SOURCE 2DROP  BEGIN REFILL WHILE INTERPRET REPEAT  SOURCE> ;

: INCLUDE  PARSE-NAME INCLUDED ;

TAG TAG

: BOOT
    SOURCE-STACK 'IN !
    ARGC $ 1 ?DO  I ARGV INCLUDED  LOOP
    TAG COUNT TYPE  QUIT ;
0 HAS BOOT


( ********** Defining Words ********** )

VARIABLE WARNING
: WARN  WARNING @ IF  >IN @  BL WORD CONTEXT @ -FIND 0= IF
    HERE COUNT TYPE ."  redefined " THEN  DROP >IN !  THEN ;

: LAST ( -- link )  CONTEXT @ CELLS  CONTEXT + ;
: PREVIOUS ( -- nfa count )  LAST @ CELL+  DUP C@ ;

: SMUDGE     PREVIOUS  $ 20 OR   SWAP C! ;
: REVEAL     PREVIOUS  $ DF AND  SWAP C! ;
: IMMEDIATE  PREVIOUS  $ 40 OR   SWAP C! ;

VARIABLE 'RECURSE

: HEADER  ( -- )  WARN  -OPT
    ALIGN  HERE  LAST  DUP @ ,  !
    BL WORD C@  1+ ALLOT  ALIGN ;

: CONSTANT  HEADER  $ 10 , , ;
: VARIABLE  HEADER  $ 11 , $ 0 , ;

\ | opc | I for does | data
: CREATE  HEADER $ 12 , ;
: DOES>   R> M -  dA @ -  $ 8 LSHIFT $ 12 OR
          PREVIOUS $ 1F AND + 1+ ALIGNED ( cfa ) ! ;
: >BODY   CELL+ ;

\ Be careful from here on...

COMPILER
: [  $ 0 STATE ! ;
: EXIT  $ 0 OP, ;
T: ;  \\ EXIT \\ [ REVEAL ; forget
: \\  $ 2 -' ABORT" ?" COMPILE, ;
: RECURSE  'RECURSE @ COMPILE, ;
FORTH

: ]  $ -1 STATE ! ;
: :NONAME  ALIGN HERE  DUP 'RECURSE !  ] ;
T: :  HEADER SMUDGE  :NONAME DROP ;