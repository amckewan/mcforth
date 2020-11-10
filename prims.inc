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

//BASE = 10;

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
//    if (verbose > 2) {
//        printf("I=%X op=%02X ", rel(I), *I);
//        printf("R=%X %X %X (%d) ", R[0], R[1], R[2], R0-R);
//        printf("S=[%d] %X %X %X %X ", S0-S, S[2], S[1], S[0], top);
//        printf("H=%X\n", HERE);
//    }
    switch (w = *I++) {

#define push *--S = top, top =
#define pop top = *S++
#define pop2 top = S[1], S += 2
#define pop3 top = S[2], S += 3
#define LOGICAL ? -1 : 0

#define NEXT        goto next;
#define LIT         *(cell*)I
#define OFFSET      *(int8_t *)I
#define BRANCH      I += OFFSET
#define NOBRANCH    I += 1
#define EXIT        I = (byte *)*R++; NEXT
/* EXIT */ case 0x00:        EXIT
/* CALL */ case 0x01:        w = *(uint16_t *)I; *--R = (cell)I + 2; I = m + w; NEXT
/* CALL32 */ case 0x02:      w = LIT; *--R = (cell)I + CELL; I = m + w; NEXT
/* BRANCH */ case 0x03:      BRANCH; NEXT
/* DO */ case 0x04:          *--R = (cell)I + OFFSET, *--R = *S, *--R = top - *S++, pop; NOBRANCH; NEXT
/* ?DO */ case 0x05:         if (top == *S) BRANCH;
else *--R = (cell)I + OFFSET,
     *--R = *S, *--R = top - *S, NOBRANCH;
S++, pop; NEXT
/* LOOP */ case 0x06:        if ((++ *R) == 0) NOBRANCH, R += 3; else BRANCH; NEXT
/* +LOOP */ case 0x07:       w = *R, *R += top;
if ((w ^ *R) < 0 && (w ^ top) < 0) NOBRANCH, R += 3;
else BRANCH; pop; NEXT
/* LIT */ case 0x08:         push LIT; I += CELL; NEXT
/* NOP */ case 0x09:         NEXT
/* S" */ case 0x0A:          push rel(I) + 1; push *I; I = litq(I); NEXT
/* ." */ case 0x0B:          I = dotq(I); NEXT
/* ABORT" */ case 0x0C:      if (!top) { I = litq(I); pop; NEXT }
show_error((char*)I, abs(HERE), abs(SOURCE));
goto abort;
/* --- */ case 0x0D: 
/* --- */ case 0x0E: 
/* --- */ case 0x0F: 
/* DOCON */ case 0x10:       push *(cell*)aligned(I); EXIT
/* DOVAR */ case 0x11:       push rel(aligned(I)); EXIT
/* DOCREATE */ case 0x12:    push rel(aligned(I)); w = *(cell*)(I - 1) >> 8;
if (w) I = abs(w); else EXIT
#define LITOP(op) top op LIT, I += CELL; NEXT
/* LIT+ */ case 0x20:        LITOP(+=)
/* LIT- */ case 0x21:        top -= LIT, I += CELL; NEXT
/* LIT* */ case 0x22:        top *= LIT, I += CELL; NEXT
/* LIT/ */ case 0x23:        top /= LIT, I += CELL; NEXT
/* LITAND */ case 0x24:      top &= LIT, I += CELL; NEXT
/* LITOR */ case 0x25:       top |= LIT, I += CELL; NEXT
/* LITXOR */ case 0x26:      top ^= LIT, I += CELL; NEXT
/* --- */ case 0x27: 
/* LIT@ */ case 0x28:        push *(cell*)(m + LIT); I += CELL; NEXT
/* LIT! */ case 0x29:        *(cell*)(m + LIT) = top, pop; I += CELL; NEXT
#define LITCOND(cond) top = (cond) LOGICAL; I += CELL; NEXT
/* --- */ case 0x30: 
/* --- */ case 0x31: 
/* --- */ case 0x32: 
/* LIT= */ case 0x33:        LITCOND(top == LIT)
/* LIT< */ case 0x34:        LITCOND(top < LIT)
/* LIT> */ case 0x35:        LITCOND(top > LIT)
/* LITU< */ case 0x36:       LITCOND((ucell)top < (ucell)LIT)
/* LITU> */ case 0x37:       LITCOND((ucell)top > (ucell)LIT)
/* --- */ case 0x38: 
/* --- */ case 0x39: 
/* --- */ case 0x3A: 
/* LIT<> */ case 0x3B:       LITCOND(top != LIT)
/* LIT>= */ case 0x3C:       LITCOND(top >= LIT)
/* LIT<= */ case 0x3D:       LITCOND(top <= LIT)
/* LITU>= */ case 0x3E:      LITCOND((ucell)top >= (ucell)LIT)
/* LITU<= */ case 0x3F:      LITCOND((ucell)top <= (ucell)LIT)
#define LITIF(cond) w = LIT, I += CELL; if (cond) NOBRANCH; else BRANCH; pop; NEXT
/* --- */ case 0x40: 
/* --- */ case 0x41: 
/* --- */ case 0x42: 
/* LIT=IF */ case 0x43:      LITIF(top == w)
/* LIT<IF */ case 0x44:      LITIF(top < w)
/* LIT>IF */ case 0x45:      LITIF(top > w)
/* LITU<IF */ case 0x46:     LITIF((ucell)top < (ucell)w)
/* LITU>IF */ case 0x47:     LITIF((ucell)top > (ucell)w)
/* --- */ case 0x48: 
/* --- */ case 0x49: 
/* --- */ case 0x4A: 
/* LIT<>IF */ case 0x4B:     LITIF(top != w)
/* LIT>=IF */ case 0x4C:     LITIF(top >= w)
/* LIT<=IF */ case 0x4D:     LITIF(top <= w)
/* LITU>=IF */ case 0x4E:    LITIF((ucell)top >= (ucell)w)
/* LITU<=IF */ case 0x4F:    LITIF((ucell)top <= (ucell)w)
#define IF(cond)  if (cond) NOBRANCH; else BRANCH
#define IF1(cond) IF(cond); pop; NEXT
#define IF2(cond) IF(cond); pop2; NEXT
/* 0=IF */ case 0x50:     IF1(top == 0)
/* 0<IF */ case 0x51:     IF1(top < 0)
/* 0>IF */ case 0x52:     IF1(top > 0)
/* =IF */ case 0x53:      IF2(*S == top)
/* <IF */ case 0x54:      IF2(*S < top)
/* >IF */ case 0x55:      IF2(*S > top)
/* U<IF */ case 0x56:     IF2((ucell)*S < (ucell)top)
/* U>IF */ case 0x57:     IF2((ucell)*S > (ucell)top)
/* ?BRANCH */ case 0x58:  IF1(top != 0)
/* 0>=IF */ case 0x59:    IF1(top >= 0)
/* 0<=IF */ case 0x5A:    IF1(top <= 0)
/* <>IF */ case 0x5B:     IF2(*S != top)
/* >=IF */ case 0x5C:     IF2(*S >= top)
/* <=IF */ case 0x5D:     IF2(*S <= top)
/* U>=IF */ case 0x5E:    IF2((ucell)*S >= (ucell)top)
/* U<=IF */ case 0x5F:    IF2((ucell)*S <= (ucell)top)
/* + */ case 0x60:          top += *S++; NEXT
/* - */ case 0x61:          top = *S++ - top; NEXT
/* * */ case 0x62:          top *= *S++; NEXT
/* / */ case 0x63:          top = *S++ / top; NEXT
/* AND */ case 0x64:        top &= *S++; NEXT
/* OR */ case 0x65:         top |= *S++; NEXT
/* XOR */ case 0x66:        top ^= *S++; NEXT
/* --- */ case 0x67: 
/* @ */ case 0x68:          top = *(cell *)(m + top); NEXT
/* ! */ case 0x69:          *(cell *)(m + top) = *S; pop2; NEXT
/* 0= */ case 0x70:         top = (top == 0) LOGICAL; NEXT
/* 0< */ case 0x71:         top = (top < 0) LOGICAL; NEXT
/* 0> */ case 0x72:         top = (top > 0) LOGICAL; NEXT
/* = */ case 0x73:          top = (*S++ == top) LOGICAL; NEXT
/* < */ case 0x74:          top = (*S++ < top) LOGICAL; NEXT
/* > */ case 0x75:          top = (*S++ > top) LOGICAL; NEXT
/* U< */ case 0x76:         top = ((ucell)*S++ < (ucell)top) LOGICAL; NEXT
/* U> */ case 0x77:         top = ((ucell)*S++ > (ucell)top) LOGICAL; NEXT
/* 0<> */ case 0x78:   top = (top != 0) LOGICAL; NEXT
/* 0>= */ case 0x79:   top = (top >= 0) LOGICAL; NEXT
/* 0<= */ case 0x7A:   top = (top <= 0) LOGICAL; NEXT
/* <> */ case 0x7B:   top = (*S++ != top) LOGICAL; NEXT
/* >= */ case 0x7C:   top = (*S++ >= top) LOGICAL; NEXT
/* <= */ case 0x7D:   top = (*S++ <= top) LOGICAL; NEXT
/* U>= */ case 0x7E:   top = ((ucell)*S++ >= (ucell)top) LOGICAL; NEXT
/* U<= */ case 0x7F:   top = ((ucell)*S++ <= (ucell)top) LOGICAL; NEXT
/* R>DROP */ case 0x80:     ++R; NEXT // don't move
/* DUP>R */ case 0x81:      *--R = top; NEXT
/* >R */ case 0x82:         *--R = top, pop; NEXT
/* R> */ case 0x83:         push *R++; NEXT
/* R@ */ case 0x84:         push *R  ; NEXT
/* DUP */ case 0x85:        *--S = top; NEXT
/* DROP */ case 0x86:       pop; NEXT
/* SWAP */ case 0x87:       w = top; top = *S; *S = w; NEXT
/* OVER */ case 0x88:       push S[1]; NEXT
/* ROT */ case 0x89:        w = S[1], S[1] = *S, *S = top, top = w; NEXT
/* NIP */ case 0x8A:        S++; NEXT
/* ?DUP */ case 0x8B:       if (top) *--S = top; NEXT
/* PICK */ case 0x8C:       top = S[top]; NEXT
/* I */ case 0x8D:          push R[0] + R[1]; NEXT
/* J */ case 0x8E:          push R[3] + R[4]; NEXT
/* LEAVE */ case 0x8F:      I = (byte*)R[2];
/* UNLOOP */ case 0x90:     R += 3; NEXT
/* INVERT */ case 0x91:  top = ~top; NEXT
/* NEGATE */ case 0x92:  top = -top; NEXT
/* LSHIFT */ case 0x93:  top = *S++ << top; NEXT
/* RSHIFT */ case 0x94:  top = ((ucell)*S++) >> top; NEXT
/* MOD */ case 0x95:  top = *S++ % top;  NEXT
#define LOWER(u1,u2)  ((uint32_t)(u1) < (uint32_t)(u2))
/* WITHIN */ case 0x96: 
  w = *S++,
  top = LOWER(*S - w, top - w) LOGICAL;
  S++;
  NEXT
/* M* */ case 0x97:  {
  int64_t d = (int64_t)*S * (int64_t)top;
  *S = d ;
  top = d >> 32;
  NEXT }
/* UM* */ case 0x98:  {
  uint64_t u1 = (uint32_t)*S;
  uint64_t u2 = (uint32_t)top;
  uint64_t ud = u1 * u2;
  *S = ud ;
  top = ud >> 32;
  NEXT }
/* UM/MOD */ case 0x99:  {
  uint64_t ud = ((uint64_t)*S << 32) | (uint32_t)S[1];
  uint64_t u = (uint32_t)top;
  uint32_t quot = ud / u;
  uint32_t rem = ud % u;
  *++S = rem;
  top = quot;
  NEXT }
/* SM/REM */ case 0x9A:  {
  int64_t d = (((uint64_t)*S) << 32) | ((uint32_t) S[1]);
  int32_t quot = d / top;
  int32_t rem = d % top;
  *++S = rem;
  top = quot;
  NEXT }
/* 1+ */ case 0x9B:     top += 1; NEXT
/* 1- */ case 0x9C:     top -= 1; NEXT
/* 2* */ case 0x9D:     top <<= 1; NEXT
/* 2/ */ case 0x9E:     top >>= 1; NEXT
/* CELLS */ case 0x9F:      top *= CELL; NEXT
/* +! */ case 0xA0:         *(cell *)(m + top) += *S; pop2; NEXT
/* C@ */ case 0xA1:   top = m[top]; NEXT
/* C! */ case 0xA2:   m[top] = *S; pop2; NEXT
/* 2@ */ case 0xA3:     *--S = *(cell*)(m + top + CELL); top = *(cell*)(m + top); NEXT
/* 2! */ case 0xA4:     *(cell*)(m + top) = *S++; *(cell*)(m + top + CELL) = *S++; pop; NEXT
#define W(a)  *(uint16_t *)(m + (a))
/* W@ */ case 0xA5:     top = W(top); NEXT
/* W! */ case 0xA6:     W(top) = *S++, pop; NEXT
/* FILL */ case 0xA7:        memset(abs(S[1]), top, *S);       pop3; NEXT
/* MOVE */ case 0xA8:   memmove(abs(*S), abs(S[1]), top); pop3; NEXT
/* >ABS */ case 0xA9:  top += (cell)m; NEXT   // convert to absolute address
/* >REL */ case 0xAA:  top -= (cell)m; NEXT   // convert to relative address
/* KEY */ case 0xAB:   push getchar(); NEXT
/* EMIT */ case 0xAC:   putchar(top); pop; NEXT
/* TYPE */ case 0xAD:    type(*S, top); pop2; NEXT
/* COMPARE */ case 0xAE:  top = compare(abs(S[2]), S[1], abs(*S), top); S += 3; NEXT
/* SEARCH */ case 0xAF:   top = search(S++, top); NEXT
/* BYE */ case 0xB0:  return 0;
/* ACCEPT */ case 0xB1:   top = accept(*S++, top);
/* ARGC */ case 0xB2:  push argc; NEXT
/* ARGV */ case 0xB3:  *--S = rel(argv[top]); top = (cell)strlen(argv[top]); NEXT
/* CREATE-FILE */ case 0xB4: 
top = (cell)open_file(abs(S[1]), *S, abs(top));
*++S = top, top = top ? 0 : -1; NEXT
/* OPEN-FILE */ case 0xB5: 
top = (cell)open_file(abs(S[1]), *S, abs(top));
*++S = top, top = top ? 0 : -1; NEXT
/* CLOSE-FILE */ case 0xB6: 
top = fclose((FILE*)top); NEXT
/* READ-FILE */ case 0xB7: 
w = fread(abs(S[1]), 1, *S, (FILE*)top);
top = w == *S ? 0 : ferror((FILE*)top); *++S = w; NEXT
/* READ-LINE */ case 0xB8: 
w = (cell)fgets(abs(S[1]), *S + 1, (FILE*)top);
if (!w) {
  top = feof((FILE*)top) ? 0 : ferror((FILE*)top);
  *S = S[1] = 0; NEXT
}
top = strlen((char*)w);
if (top > 0 && ((char*)w)[top-1] == '\n') --top;
S[1] = top, *S = TRUE, top = 0; NEXT
/* WRITE-FILE */ case 0xB9: 
w = fwrite(abs(S[1]), 1, *S, (FILE*)top);
top = w == *S ? 0 : ferror((FILE*)top); S += 2; NEXT
/* WRITE-LINE */ case 0xBA: 
w = fwrite(abs(S[1]), 1, *S, (FILE*)top);
if (w == *S) *S = 1, w = fwrite("\n", 1, 1, (FILE*)top);
top = w == *S ? 0 : ferror((FILE*)top); S += 2; NEXT
/* ALLOCATE */ case 0xBB:   *--S = rel(malloc(top)), top = *S ? 0 : -1; NEXT
/* RESIZE */ case 0xBC:     *S = rel(realloc(abs(*S), top)), top = *S ? 0 : -1; NEXT
/* FREE */ case 0xBD:       if (top) free(abs(top)); top = 0; NEXT
/* NEW-STRING */ case 0xBE: top = new_string(*S++, top); NEXT
/* REFILL */ case 0xBF:   push refill(SOURCE); NEXT
/* . */ case 0xC0:   printf("%d ", top); pop; NEXT
/* -NUMBER */ case 0xC1:  w = number(abs(top), --S, BASE);
  if (w) top = 0; else *S = top, top = -1; NEXT
/* >NUMBER */ case 0xC2:  top = to_number(S, top, BASE); NEXT
/* WORD */ case 0xC3: 
  top = word(SOURCE, top, HERE); NEXT
/* FIND */ case 0xC4: 
      w = find(top, M[CONTEXT + 1]);
      if (w > 0) *--S = w, top = -1;
      else if (w < 0) *--S = -w, top = 1;
      else push 0; NEXT
/* -FIND */ case 0xC5: 
      w = find(*S, M[CONTEXT + top]);
      if (w) *S = w < 0 ? -w : w, top = 0;
      else top = -1; NEXT
/* >NAME */ case 0xC6:   top = xt_to_name(top); NEXT
/* NAME> */ case 0xC7:   top = name_to_xt(top); NEXT
/* DEPTH */ case 0xC8:   w = S0 - S; push w; NEXT
/* .S */ case 0xC9: 
      w = S0 - S;  S[-1] = top;
      printf("[%d] ", w);
      for (int i = w - 2; i >= -1; i--)
          printf("%d (0x%x) ", S[i], S[i]);
      NEXT
/* WORDS */ case 0xCA:   words(M[CONTEXT + M[CONTEXT]]); NEXT
/* DUMP */ case 0xCB:   dump(*S++, top, BASE); pop; NEXT
/* VERBOSE */ case 0xCC:  push (byte *)&verbose - m; NEXT
/* ALIGNED */ case 0xCD:  top = aligned(top); NEXT
/* EXECUTE */ case 0xCE:  *--R = (cell)I, I = m + top, pop; NEXT
/* R0! */ case 0xCF:  R = R0; NEXT
/* PARSE */ case 0xD0:   top = parse(SOURCE, top, --S); NEXT
/* PARSE-NAME */ case 0xD1:   push parse_name(SOURCE, --S); NEXT
