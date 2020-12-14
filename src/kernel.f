\ Forth Kernel
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

( COLD ) 0 ,  ( WARM ) 0 ,
( H ) 0 ,  ( BASE ) #10 ,  ( STATE ) 0 ,  ( 'IN ) 0 ,
( FORTH ) HERE 0 , 0 ,
( CURRENT ) DUP ,A  ( CONTEXT ) ,A  0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
( HANDLER) 0 , 8 ,
( MSG ) 0 ,

2 +ORIGIN CONSTANT H
3 +ORIGIN CONSTANT BASE
4 +ORIGIN CONSTANT STATE
5 +ORIGIN CONSTANT 'IN
6 +ORIGIN CONSTANT FORTH-WORDLIST
8 +ORIGIN CONSTANT CURRENT
9 +ORIGIN CONSTANT CONTEXT
12 +ORIGIN CONSTANT HANDLER
14 +ORIGIN CONSTANT MSG

``
#define COLD M[0]
#define WARM M[1]
#define HERE M[2]
#define BASE M[3]
#define STATE M[4]
#define SOURCE M[5]
#define FORTH 6
#define CURRENT 8
#define CONTEXT 9
#define HANDLER M[0x12]
#define RESUME M[0x13]
#define MSG M[0x14]

byte *I;
cell *S, top;
cell *R;
cell w;

#define LOCALS 1
#if LOCALS
cell *L;
#endif

#define OBJECTS 0
#if OBJECTS
cell self;
#endif

// if (verbose) printf("Running from %u\n", COLD);
I = abs(COLD);
goto start;

abort:
    show_error(top, MSG ? abs(MSG) : 0, abs(HERE), abs(SOURCE));
    I = abs(WARM);
start:
    STATE = 0;
    M[CURRENT] = M[CONTEXT] = CELLS(FORTH);
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

#define THROW       R = (cell*) HANDLER, \
                    HANDLER = *R++, S = (cell*) *R++, I = (byte*) *R++
``

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

OP: RESUME      HANDLER = *R++, R++, I = (byte*) *R++, push 0; NEXT
OP: JUMP        w = *(uint16_t *)I; I = m + CELLS(w); NEXT
OP: S"          w = *I++, push rel(I), push w, I += w; NEXT
OP: ."          I = dotq(I); NEXT
OP: ABORT"      if (!top) { w = *I++, I += w, pop; NEXT }
                ` top = -2, MSG = rel(I); if (!HANDLER) goto abort; THROW; NEXT

10 OP! ( runtime for defining words )

OP: DOCON       push at(aligned(I)); EXIT
OP: DOVAR       push rel(aligned(I)); EXIT
OP: DODOES      push rel(aligned(I)); I = abs(at(I - 1) >> 8); NEXT
OP: DOVALUE     push at(aligned(I)); EXIT
OP: DODEFER     w = at(aligned(I)); I = m + w; NEXT

\ OP: DOSELECTOR  w = find_method(AT(top - CELL), aligned(I));
\                 ` if (w) { *--R = (cell)I, I = m + w, pop; NEXT }
\                 ` not_understood: ABORT("\x16message not understood")
\ CODE BIND  ( sel class -- xt )
\     ` top = find_method(top, *S++);
\     ` if (!top) goto not_understood; NEXT;
\ OP: DOOBJECT    push rel(aligned(I)) + CELL; EXIT


18 OP!

\ Local frame
\ L --> old L
\       local#1
\       local#2
\       local#n

` #if LOCALS
OP: L{          *--R = (cell)L, L = R;
                ` w = *I++; while (w--) *--R = 0;
                ` w = *I++; while (w--) *--R = top, pop;
                ` NEXT
OP: }L          R = L + 1, L = (cell *)*L; NEXT
OP: L@          w = *I++, push L[-w]; NEXT
OP: L!          w = *I++, L[-w] = top, pop; NEXT
` #endif

\ OP: ENTERM      *--R = self, self = top, pop; NEXT
\ OP: EXITM       self = *R++; NEXT

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
\ OP: IVAR        push self + LIT; I += CELL; NEXT

30 OP! ( lit cond : op | lit )

\ not needed for 0= 0< etc. so this frees up 6 slots, but be careful!
\ compile, assumes these are literals corresponding to 7x ops
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
\ not needed for 0= 0< etc. so this frees up 6 slots, but be careful!

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

OP: NOP         NEXT
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
---
---
---
---
---

70 OP! ( conditionals )

` #define COND(cond) top = (cond) LOGICAL; NEXT

CODE 0=         COND(top == 0)
CODE 0<         COND(top < 0)
CODE 0>         COND(top > 0)
CODE =          COND(*S++ == top)
CODE <          COND(*S++ < top)
CODE >          COND(*S++ > top)
CODE U<         COND((ucell)*S++ < (ucell)top)
CODE U>         COND((ucell)*S++ > (ucell)top)

OP: 0<>         COND(top != 0)
OP: 0>=         COND(top >= 0)
OP: 0<=         COND(top <= 0)
OP: <>          COND(*S++ != top)
OP: >=          COND(*S++ >= top)
OP: <=          COND(*S++ <= top)
OP: U>=         COND((ucell)*S++ >= (ucell)top)
OP: U<=         COND((ucell)*S++ <= (ucell)top)

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

CODE R>DROP     ++R; NEXT
CODE DUP>R      *--R = top; NEXT

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

CODE (BYE)  return top;
: BYE $ 0 (BYE) ;

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

8 CELLS CONSTANT #SOURCE ( size of each source entry )

\ 8 entries * 8 cells per entry
40 CELLS BUFFER SOURCE-STACK

: CELLS  CELL * ;

: >IN           'IN @ ;
: SOURCE-BUF    >IN $ 2 CELLS + ;
: SOURCE-FILE   >IN $ 3 CELLS + ;
: SOURCE-NAME   >IN $ 4 CELLS + ;
: SOURCE-LINE   >IN $ 5 CELLS + ;

: SOURCE        >IN CELL+ 2@ ;
: SOURCE-ID     SOURCE-FILE @ ;

: SOURCE-DEPTH  >IN SOURCE-STACK -  #SOURCE / ;

: FILE? ( source-id -- f )  1+ $ 1 U> ;

: >SOURCE ( filename len fileid | -1 -- )
    SOURCE-DEPTH $ 7 U> ABORT" nested too deep"
    #SOURCE 'IN +!
    DUP SOURCE-FILE !
    FILE? IF  $ 80 ALLOCATE DROP SOURCE-BUF !  NEW-STRING SOURCE-NAME !  THEN
    $ 0 SOURCE-LINE ! ;

: SOURCE> ( -- )
    SOURCE-DEPTH $ 1 < ABORT" trying to pop empty source"
    SOURCE-ID FILE? IF
        SOURCE-ID CLOSE-FILE DROP
        SOURCE-BUF  @ FREE DROP
        SOURCE-NAME @ FREE DROP
    THEN
    #SOURCE NEGATE 'IN +! ;

CODE REFILL ( -- f )  push refill(SOURCE); NEXT

\ ********** Numbers **********

` #define dot(n)  printf(BASE == 16 ? "%tx " : "%td ", n)

CODE .  ( n -- )  dot(top), pop; NEXT

CODE -NUMBER  ( a -- a t, n f ) w = number(abs(top), --S, BASE);
`   if (w) top = 0; else *S = top, top = -1; NEXT
: NUMBER  ( a -- n )  -NUMBER ABORT" ? " ;

CODE >NUMBER  top = to_number(S, top, BASE); NEXT

\ ********** Parsing **********

20 CONSTANT BL

CODE PARSE    ( c -- a n )  top = parse(SOURCE, top, --S); NEXT
CODE PARSE-NAME ( -- a n )  push parse_name(SOURCE, --S); NEXT

CODE WORD  ( char -- addr )
`   top = word(SOURCE, top, HERE); NEXT

\ ********** Dictionary search **********

CODE SEARCH-WORDLIST  ( c-addr u wid -- 0 | xt 1 | xt -1 )
    ` w = search_wordlist(S[1], S[0], top);
    ` if (w > 0) *++S = w, top = -1;
    ` else if (w < 0) *++S = -w, top = 1;
    ` else S += 2, top = 0; NEXT

\ CODE xFIND  ( str -- xt flag | str 0 )
\ `       w = find(top, CELLS(CONTEXT)); // search FORTH only
\ `       if (w > 0) *--S = w, top = -1;
\ `       else if (w < 0) *--S = -w, top = 1;
\ `       else push 0; NEXT

: FIND  ( c-addr -- c-addr 0 | xt 1 | xt -1 )
    DUP COUNT FORTH-WORDLIST SEARCH-WORDLIST
    DUP IF  ROT DROP  THEN ;

: '  ( --- xt )  BL WORD FIND 0= ABORT" ?" ;

CODE >NAME ( xt -- nfa )  top = xt_to_name(top); NEXT
CODE NAME> ( nfa -- xt )  top = name_to_xt(top); NEXT

CODE DEPTH ( -- n )  w = S0 - S; push w; NEXT
CODE .S ( -- )
    ` w = S0 - S; if (w <= 0) { printf("empty "); NEXT }
    ` S[-1] = top;
    ` for (w -= 2; w >= -1; w--) dot(S[w]);
    ` NEXT

CODE WORDS  ( -- )  words(M[CONTEXT]); NEXT
CODE DUMP  ( a n -- )  dump(*S++, top, BASE); pop; NEXT
CODE VERBOSE  push rel(&verbose); NEXT

( ********** Catch/Throw ********** )

CODE EXECUTE ( xt -- )  *--R = (cell)I, I = m + top, pop; NEXT

CODE CATCH  ( xt -- ex# | 0 )
    ` *--R = (cell) I, *--R = (cell) S, *--R = HANDLER, HANDLER = (cell) R,
    ` *--R = (cell) &RESUME, I = m + top, pop; NEXT

CODE THROW  ( n -- )
    ` if (!top) pop; else if (!HANDLER) goto abort; else THROW; NEXT

CODE RESET  R = R0, HANDLER = 0; NEXT

( ********** Compiler ********** )

VARIABLE dA ( offset for target compiler )
VARIABLE ?CODE 0 ,

: -OPT  $ 0 ?CODE ! ;

CODE UNUSED  ( -- u )  push rel(R0) - CELLS(256) - HERE; NEXT

: HERE   H @  ;
: ALLOT  H +! ;
: ,   HERE !  CELL H +! ;
: C,  HERE C!  $ 1 H +! ;
: W,  HERE W!  $ 2 H +! ;

CODE ALIGNED  top = aligned(top); NEXT
: ALIGN  BEGIN HERE DUP ALIGNED < WHILE $ 0 C, REPEAT ;

: OP, ( opc -- )  ?CODE @ HERE ?CODE 2!  C, ;

: LITERAL  $ 20 OP, , ; IMMEDIATE

: LATEST ( -- op | 0 )  ?CODE @ DUP IF C@ THEN ;
: PATCH  ( op -- )      ?CODE @ C! ;
: REMOVE ( -- )         $ 0 ?CODE 2@  H !  ?CODE 2! ;

: LIT?  ( -- f )  ?CODE @ DUP IF  C@ $ 20 =  THEN ;
: LIT@  ( -- n )  ?CODE @ $ 1 + @ ;
: LIT!  ( n -- )  ?CODE @ $ 1 + ! ;

: BINARY ( op -- ) \ e.g. lit +
    LIT? IF  LIT@ REMOVE
        LIT? IF  LIT@ SWAP ROT ( n1 n2 op )
            HERE !  HERE EXECUTE  LIT!
        ELSE
            SWAP $ 40 XOR OP, ,
        THEN
    ELSE  OP,
    THEN ;

: MEMORY  ( op -- ) \ e.g lit @
    LIT? IF  $ 40 XOR PATCH  ELSE  OP,  THEN ;

: NOT,  ( op -- )  \ invert last conditional op
    LATEST  DUP $ 70 $ 80 WITHIN  OVER $ F7 AND $ 33 $ 38 WITHIN OR
    IF  $ 8 XOR PATCH DROP  ELSE  DROP OP,  THEN ;

: PACK ( opc -- ) \ peephole optimizer
    DUP $ 60 $ 80 WITHIN
    IF  DUP $ 68 $ 6B WITHIN IF  MEMORY EXIT  THEN
        DUP $ 70 =           IF  NOT,   EXIT  THEN
        BINARY EXIT
    THEN OP, ;

: LITOP ( xt -- )
    COUNT  SWAP @ [COMPILE] LITERAL  $ 40 XOR PACK ;

: INLINE?  ( xt -- n t | f ) \ count ops >= $60
    DUP BEGIN  DUP C@ WHILE
        COUNT $ 60 < IF  2DROP $ 0 EXIT  THEN
    REPEAT SWAP - $ -1 ;

: INLINE ( xt n -- ) $ 0 ?DO  COUNT PACK  LOOP DROP ;

: COMPILE,  ( xt -- )
    \ inline primatives
    DUP INLINE? IF INLINE EXIT THEN

    \ inline constant etc.
    DUP C@ $ 10 = IF ( constant ) CELL+ @      [COMPILE] LITERAL  EXIT THEN
    DUP C@ $ 11 = IF ( variable ) CELL+ dA @ - [COMPILE] LITERAL  EXIT THEN
    DUP C@ $ 13 = IF ( value )    CELL+ dA @ - $ 28 OP, ,         EXIT THEN

    \ inline lit op exit (e.g. 1+, HERE)
    DUP COUNT $ 20 $ 40 WITHIN  SWAP CELL+ C@ 0= AND IF  LITOP EXIT  THEN

\ Optional check for bad behavior!
\    DUP CELL 1- AND ABORT" xt not aligned"

    \ compile short call in first 64k cells
    DUP $ 10000 CELLS U< IF  $ 1 OP, dA @ - CELL / W,  EXIT THEN

    \ default to far call
    $ 2 OP, dA @ - , ;

( optimize tail calls )
: EXIT  LATEST $ 1 = IF  $ 9 PATCH  ELSE  $ 0 OP,  THEN ; IMMEDIATE

( ********** Interpreter ********** )

: ?STACK  DEPTH 0< ABORT" stack?" ;

: INTERPRET  ( -- )
    BEGIN  BL WORD  DUP C@ WHILE
        FIND ?DUP IF
            STATE @ = IF  COMPILE,  ELSE  EXECUTE  ?STACK  THEN
        ELSE
            NUMBER  STATE @ IF  [COMPILE] LITERAL  THEN
        THEN
    REPEAT DROP ;

: (INCLUDE)  BEGIN REFILL WHILE INTERPRET REPEAT ;

: INCLUDE-FILE  ( str len fid -- )  >SOURCE  HANDLER @
    IF  ['] (INCLUDE) CATCH SOURCE> THROW  ELSE  (INCLUDE) SOURCE>  THEN ;

: INCLUDED  ( str len -- )
    2DUP R/O OPEN-FILE ABORT" file not found" INCLUDE-FILE ;

: INCLUDE  PARSE-NAME INCLUDED ;

80 BUFFER TIB
: QUERY  $ 0 SOURCE-FILE !  TIB SOURCE-BUF !  REFILL 0= IF BYE THEN ;

: QUIT  RESET
    BEGIN  SOURCE-DEPTH WHILE  SOURCE>  REPEAT
    BEGIN  CR QUERY  INTERPRET  STATE @ 0= IF ."  ok" THEN  AGAIN ;
1 HAS QUIT

TAG TAG

: COLD
    SOURCE-STACK 'IN !
    ARGC $ 1 ?DO  I ARGV INCLUDED  LOOP
    TAG COUNT TYPE  QUIT ;
0 HAS COLD

( ********** Defining Words ********** )

VARIABLE WARNING
: WARN  WARNING @ IF  >IN @  BL WORD FIND IF
    HERE COUNT TYPE ."  redefined " THEN  DROP >IN !  THEN ;

VARIABLE LAST 0,
: PRIOR ( -- nfa count )  LAST @ CELL+  DUP C@ ;

: HIDE      LAST @ @  CURRENT @ ! ;
: REVEAL    LAST @ ?DUP IF  CURRENT @ !  THEN ;

: LINK,     ALIGN HERE  OVER @ ,  SWAP ! ;
: HEADER    WARN  ALIGN HERE  CURRENT @ LINK,
            BL WORD C@ 1+ ALLOT  ALIGN  HERE SWAP LAST 2!  -OPT ;

: CONSTANT  HEADER  $ 10 , , ;
: CREATE    HEADER  $ 11 , ;
: VARIABLE  CREATE  $ 0 , ;

\ | opc | I for does | data
: DOES>   R> M -  dA @ -  $ 8 LSHIFT $ 12 OR  LAST CELL+ @ ! ;
: >BODY   CELL+ ;

\ Be careful from here on...

: [  $ 0 STATE ! ; IMMEDIATE
T: ;  [COMPILE] EXIT [COMPILE] [ REVEAL ; IMMEDIATE forget
: RECURSE  LAST CELL+ @ COMPILE, ; IMMEDIATE

: ]  $ -1 STATE ! ;
: :NONAME  ALIGN HERE  DUP $ 0 LAST 2!  -OPT  ] ;
: :  HEADER HIDE ] ;

``
default:
    printf("Invalid opcode 0x%02X\n", I[-1]);
    top = -256;
    goto abort;
}
``
