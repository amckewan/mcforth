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
/* --- */ case 0x09:         NEXT
/* S" */ case 0x0A:          push rel(I) + 1; push *I; I = litq(I); NEXT
/* ." */ case 0x0B:          I = dotq(I); NEXT
/* ABORT" */ case 0x0C:      if (!top) { I = litq(I); pop; NEXT }
show_error((char*)I, abs(HERE), abs(SOURCE));
goto abort;
/* DOCON */ case 0x10:       push LIT; EXIT
/* DOVAR */ case 0x11:       push aligned(rel(I)); EXIT
/* DOCREATE */ case 0x12:    push aligned(rel(I)) + CELL; w = *(cell*)aligned(I);
if (w) I = abs(w); else EXIT
/* LIT+ */ case 0x20:   top += *I++; NEXT
/* LIT- */ case 0x21:    top -= *I++; NEXT
/* LIT-AND */ case 0x22:  top &= *I++; NEXT
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
/* ?BRANCH */ case 0x58:       IF1(top != 0)
/* 0>=IF */ case 0x59:    IF1(top >= 0)
/* 0<=IF */ case 0x5A:    IF1(top <= 0)
/* <>IF */ case 0x5B:     IF2(*S != top)
/* >=IF */ case 0x5C:     IF2(*S >= top)
/* <=IF */ case 0x5D:     IF2(*S <= top)
/* U>=IF */ case 0x5E:    IF2((ucell)*S >= (ucell)top)
/* U<=IF */ case 0x5F:    IF2((ucell)*S <= (ucell)top)
/* + */ case 0x60:          top += *S++; NEXT
/* - */ case 0x61:          top = *S++ - top; NEXT
/* AND */ case 0x62:        top &= *S++; NEXT
/* OR */ case 0x63:         top |= *S++; NEXT
/* XOR */ case 0x64:        top ^= *S++; NEXT
/* LSHIFT */ case 0x65:     top = *S++ << top; NEXT
/* RSHIFT */ case 0x66:     top = ((ucell)*S++) >> top; NEXT
/* PICK */ case 0x67:       top = S[top]; NEXT
/* @ */ case 0x68:          top = *(cell *)(m + top); NEXT
/* ! */ case 0x69:          *(cell *)(m + top) = *S; pop2; NEXT
/* +! */ case 0x6A:         *(cell *)(m + top) += *S; pop2; NEXT
/* * */ case 0x6B:          top *= *S++; NEXT
/* / */ case 0x6C:          top = *S++ / top; NEXT
/* NOP */ case 0x6D:        NEXT
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
/* )<= */ case 0x7A:   top = (top <= 0) LOGICAL; NEXT
/* <> */ case 0x7B:   top = (*S++ != top) LOGICAL; NEXT
/* >= */ case 0x7C:   top = (*S++ >= top) LOGICAL; NEXT
/* <= */ case 0x7D:   top = (*S++ <= top) LOGICAL; NEXT
/* U>= */ case 0x7E:   top = ((ucell)*S++ >= (ucell)top) LOGICAL; NEXT
/* U<= */ case 0x7F:   top = ((ucell)*S++ <= (ucell)top) LOGICAL; NEXT
/* DUP */ case 0x80:        *--S = top; NEXT
/* DROP */ case 0x81:       pop; NEXT
/* SWAP */ case 0x82:       w = top; top = *S; *S = w; NEXT
/* OVER */ case 0x83:       push S[1]; NEXT
/* ROT */ case 0x84:        w = S[1], S[1] = *S, *S = top, top = w; NEXT
/* NIP */ case 0x85:        S++; NEXT
/* ?DUP */ case 0x86:       if (top) *--S = top; NEXT
/* >R */ case 0x87:         *--R = top, pop; NEXT
/* R> */ case 0x88:         push *R++; NEXT
/* R@ */ case 0x89:         push *R  ; NEXT
/* I */ case 0x8A:          push R[0] + R[1]; NEXT
/* J */ case 0x8B:          push R[3] + R[4]; NEXT
/* LEAVE */ case 0x8C:      I = (byte*)R[2];
/* UNLOOP */ case 0x8D:     R += 3; NEXT
/* INVERT */ case 0x8E:  top = ~top; NEXT
/* NEGATE */ case 0x8F:  top = -top; NEXT
/* MOD */ case 0x90:  top = *S++ % top;  NEXT
#define LOWER(u1,u2)  ((uint32_t)(u1) < (uint32_t)(u2))
/* WITHIN */ case 0x91: 
  w = *S++,
  top = LOWER(*S - w, top - w) LOGICAL;
  S++;
  NEXT
/* M* */ case 0x92:  {
  int64_t d = (int64_t)*S * (int64_t)top;
  *S = d ;
  top = d >> 32;
  NEXT }
/* UM* */ case 0x93:  {
  uint64_t u1 = (uint32_t)*S;
  uint64_t u2 = (uint32_t)top;
  uint64_t ud = u1 * u2;
  *S = ud ;
  top = ud >> 32;
  NEXT }
/* UM/MOD */ case 0x94:  {
  uint64_t ud = ((uint64_t)*S << 32) | (uint32_t)S[1];
  uint64_t u = (uint32_t)top;
  uint32_t quot = ud / u;
  uint32_t rem = ud % u;
  *++S = rem;
  top = quot;
  NEXT }
/* SM/REM */ case 0x95:  {
  int64_t d = (((uint64_t)*S) << 32) | ((uint32_t) S[1]);
  int32_t quot = d / top;
  int32_t rem = d % top;
  *++S = rem;
  top = quot;
  NEXT }
/* 1+ */ case 0x96:     top += 1; NEXT
/* 1- */ case 0x97:     top -= 1; NEXT
/* 2* */ case 0x98:     top <<= 1; NEXT
/* 2/ */ case 0x99:     top >>= 1; NEXT
/* CELLS */ case 0x9A:      top *= CELL; NEXT
/* C@ */ case 0x9B:   top = m[top]; NEXT
/* C! */ case 0x9C:   m[top] = *S; pop2; NEXT
#define W(a)  *(unsigned short *)(m + (a))
/* W@ */ case 0x9D:     top = W(top); NEXT
/* W! */ case 0x9E:     W(top) = *S; pop2; NEXT
/* FILL */ case 0x9F:   memset(abs(S[1]), top, *S); pop3; NEXT
/* MOVE */ case 0xA0:   memmove(abs(*S), abs(S[1]), top); pop3; NEXT
/* >ABS */ case 0xA1:  top += (cell)m; NEXT   // convert to absolute address
/* >REL */ case 0xA2:  top -= (cell)m; NEXT   // convert to relative address
/* KEY */ case 0xA3:   push getchar(); NEXT
/* EMIT */ case 0xA4:   putchar(top); pop; NEXT
/* TYPE */ case 0xA5:    type(*S, top); pop2; NEXT
/* COMPARE */ case 0xA6:  top = compare(abs(S[2]), S[1], abs(*S), top); S += 3; NEXT
/* BYE */ case 0xA7:  return 0;
/* ACCEPT */ case 0xA8:   top = accept(*S++, top);
/* FIXME */ if (top < 0) exit(0); NEXT
/* ARGC */ case 0xA9:  push argc; NEXT
/* ARGV */ case 0xAA:  *--S = rel(argv[top]); top = (cell)strlen(argv[top]); NEXT
/* CREATE-FILE */ case 0xAB: 
top = (cell)open_file(abs(S[1]), *S, abs(top));
*++S = top, top = top ? 0 : -1; NEXT
/* OPEN-FILE */ case 0xAC: 
top = (cell)open_file(abs(S[1]), *S, abs(top));
*++S = top, top = top ? 0 : -1; NEXT
/* CLOSE-FILE */ case 0xAD: 
top = fclose((FILE*)top); NEXT
/* READ-FILE */ case 0xAE: 
w = fread(abs(S[1]), 1, *S, (FILE*)top);
top = w == *S ? 0 : ferror((FILE*)top); *++S = w; NEXT
/* READ-LINE */ case 0xAF: 
w = (cell)fgets(abs(S[1]), *S + 1, (FILE*)top);
if (!w) {
  top = feof((FILE*)top) ? 0 : ferror((FILE*)top);
  *S = S[1] = 0; NEXT
}
top = strlen((char*)w);
if (top > 0 && ((char*)w)[top-1] == '\n') --top;
S[1] = top, *S = TRUE, top = 0; NEXT
/* WRITE-FILE */ case 0xB0: 
w = fwrite(abs(S[1]), 1, *S, (FILE*)top);
top = w == *S ? 0 : ferror((FILE*)top); S += 2; NEXT
/* WRITE-LINE */ case 0xB1: 
w = fwrite(abs(S[1]), 1, *S, (FILE*)top);
if (w == *S) *S = 1, w = fwrite("\n", 1, 1, (FILE*)top);
top = w == *S ? 0 : ferror((FILE*)top); S += 2; NEXT
/* ALLOCATE */ case 0xB2:   *--S = rel(malloc(top)), top = *S ? 0 : -1; NEXT
/* RESIZE */ case 0xB3:     *S = rel(realloc(abs(*S), top)), top = *S ? 0 : -1; NEXT
/* FREE */ case 0xB4:       if (top) free(abs(top)); top = 0; NEXT
/* NEW-STRING */ case 0xB5: top = new_string(*S++, top); NEXT
/* REFILL */ case 0xB6:   push refill(SOURCE); NEXT
/* . */ case 0xB7:   printf("%d ", top); pop; NEXT
/* -NUMBER */ case 0xB8:  w = number(abs(top), --S);
  if (w) top = 0; else *S = top, top = -1; NEXT
/* >NUMBER */ case 0xB9:  top = to_number(S, top); NEXT
/* WORD */ case 0xBA: 
  top = word(SOURCE, top, HERE); NEXT
/* FIND */ case 0xBB: 
      w = find(top, 1);
      if (w) *--S = cfa(w), top = -1;
      else push 0; NEXT
/* -FIND */ case 0xBC: 
      w = find(*S, top);
      if (w) *S = cfa(w), top = 0;
      else top = -1; NEXT
/* >NAME */ case 0xBD:   top = nfa(top); NEXT
/* NAME> */ case 0xBE:   top = cfa(top); NEXT
/* DEPTH */ case 0xBF:   w = S0 - S; push w; NEXT
/* .S */ case 0xC0: 
      w = S0 - S;  S[-1] = top;
      printf("[%d] ", w);
      for (int i = w - 2; i >= -1; i--)
          printf("%d (0x%x) ", S[i], S[i]);
      NEXT
/* WORDS */ case 0xC1:   words(M[CONTEXT]); NEXT
/* DUMP */ case 0xC2:   dump(*S++, top); pop; NEXT
/* EXECUTE */ case 0xC3:  *--R = (cell)I, I = m + top, pop; NEXT
/* ALIGNED */ case 0xC4:    top = aligned(top); NEXT
/* PARSE */ case 0xC5:   top = parse(SOURCE, top, --S); NEXT
/* PARSE-NAME */ case 0xC6:   push parse_name(SOURCE, --S); NEXT
