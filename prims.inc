#define S sp
#define R rp
#define I ip
case 0x000:  /* EXIT */  xit:  ip = (opcode*) *rp--; NEXT
/* EXECUTEX */ case 0x001:        w = top; pop; goto exec;
#define OFFSET    *(cell*)I
#define BRANCH    I += OFFSET
#define NOBRANCH  I += CELL
case 0x002:  /* BRANCH */    BRANCH; NEXT
case 0x003:  /* DO */        *++R = (cell)I + OFFSET, *++R = *S, *++R = top - *S--, pop;
                  //printf("DO R=%p I=%d %d\n", R, R[0], R[-1]);
                  NOBRANCH; NEXT
case 0x004:  /* ?DO */       if (top == *S) BRANCH;
                  else *++R = (cell)I + OFFSET,
                      *++R = *S, *++R = top - *S, NOBRANCH;
                  S--, pop; NEXT
case 0x005:  /* LOOP */      //printf("LOOP R=%p I=%d %d\n", R, R[0], R[-1]);
                  if ((++ *R) == 0) NOBRANCH, R -= 3;
                  else BRANCH; NEXT 
case 0x006:  /* +LOOP */     w = *R, *R += top;
                  if ((w ^ *R) < 0 && (w ^ top) < 0) NOBRANCH, R -= 3;
                  else BRANCH; pop; NEXT
case 0x007:  /* DLIT */      push *ip++; push *ip++; NEXT
case 0x008:  /* DOVAR */     push (uchar*)ip++ - m; goto xit;
case 0x009:  /* ." */        ip = dotq(ip); NEXT
case 0x00A:  /* S" */        push virt(ip) + 1; push *ip; ip = litq(ip); NEXT
case 0x00B:  /* abort" */    if (top) {
  show_error((char*)ip, phys(HERE), phys(SOURCE));
  goto abort;
} ip = litq(ip); pop; NEXT
/* + */ case 0x010:           top += *sp--; NEXT
/* - */ case 0x011:           top = *sp-- - top; NEXT
/* AND */ case 0x012:         top &= *sp--; NEXT
/* OR */ case 0x013:          top |= *sp--; NEXT
/* XOR */ case 0x014:         top ^= *sp--; NEXT
/* LSHIFT */ case 0x015:      top = *sp-- << top; NEXT
/* RSHIFT */ case 0x016:      top = (cell) ((ucell)(*sp--) >> top); NEXT
/* ARSHIFT */ case 0x017:     top = *sp-- >> top; NEXT
/* SWAP */ case 0x018:        w = top; top = *sp; *sp = w; NEXT
/* PICK */ case 0x019:        top = sp[-top]; NEXT
/* @ */ case 0x01A:           top = M(top); NEXT
/* ! */ case 0x01B:           M(top) = *sp; pop2; NEXT
/* +! */ case 0x01C:          M(top) += *sp; pop2; NEXT
/* * */ case 0x01D:           top *= *sp--; NEXT
/* / */ case 0x01E:           top = *sp-- / top; NEXT
/* NOP */ case 0x01F:       ; NEXT
case 0x020:     top += *ip++; NEXT
case 0x021:     top -= *ip++; NEXT
case 0x022:   top &= *ip++; NEXT
case 0x02F:      push *(cell*)ip; ip += CELL; NEXT
/* 0= */ case 0x040:          top = (top == 0) LOGICAL; NEXT
/* 0< */ case 0x041:          top = (top < 0) LOGICAL; NEXT
/* 0> */ case 0x042:          top = (top > 0) LOGICAL; NEXT
/* = */ case 0x043:           top = (*sp-- == top) LOGICAL; NEXT
/* < */ case 0x044:           top = (*sp-- < top) LOGICAL; NEXT
/* > */ case 0x045:           top = (*sp-- > top) LOGICAL; NEXT
/* U< */ case 0x046:          top = ((ucell)*sp-- < (ucell)top) LOGICAL; NEXT
/* U> */ case 0x047:          top = ((ucell)*sp-- > (ucell)top) LOGICAL; NEXT
case 0x048:  /* 0<> */   top = (top != 0) LOGICAL; NEXT
case 0x049:  /* 0>= */   top = (top >= 0) LOGICAL; NEXT
case 0x04A:  /* 0<= */   top = (top <= 0) LOGICAL; NEXT
case 0x04B:  /* <>  */   top = (*sp-- != top) LOGICAL; NEXT
case 0x04C:  /* >=  */   top = (*sp-- >= top) LOGICAL; NEXT
case 0x04D:  /* <=  */   top = (*sp-- <= top) LOGICAL; NEXT
case 0x04E:  /* U>= */   top = ((ucell)*sp-- >= (ucell)top) LOGICAL; NEXT
case 0x04F:  /* U<= */   top = ((ucell)*sp-- <= (ucell)top) LOGICAL; NEXT
#define IF(cond)  if (cond) NOBRANCH; else BRANCH
#define IF1(cond) IF(cond); pop; NEXT
#define IF2(cond) IF(cond); pop2; NEXT
case 0x050:  /* 0= IF */     IF1(top == 0)
case 0x051:  /* 0< IF */     IF1(top < 0)
case 0x052:  /* 0> IF */     IF1(top > 0)
case 0x053:  /* = IF */      IF2(*S == top)
case 0x054:  /* < IF */      IF2(*S < top)
case 0x055:  /* > IF */      IF2(*S > top)
case 0x056:  /* U< IF */     IF2((ucell)*S < (ucell)top)
case 0x057:  /* U> IF */     IF2((ucell)*S > (ucell)top)
case 0x058:  /* 0<> IF */    IF1(top != 0)
case 0x059:  /* 0>= IF */    IF1(top >= 0)
case 0x05A:  /* 0<= IF */    IF1(top <= 0)
case 0x05B:  /* <> IF */     IF2(*S != top)
case 0x05C:  /* >= IF */     IF2(*S >= top)
case 0x05D:  /* <= IF */     IF2(*S <= top)
case 0x05E:  /* U>= IF */    IF2((ucell)*S >= (ucell)top)
case 0x05F:  /* U<= IF */    IF2((ucell)*S <= (ucell)top)
/* DROP */ case 0x060:        pop; NEXT
/* DUP */ case 0x061:         *++sp = top; NEXT
/* NIP */ case 0x062:         sp--; NEXT
/* ?DUP */ case 0x063:        if (top) *++sp = top; NEXT
/* OVER */ case 0x064:        push sp[-1]; NEXT
/* ROT */ case 0x065:         w = S[-1], S[-1] = *S, *S = top, top = w; NEXT
/* >R */ case 0x068:          
   //printf(">R R=%p top=0x%X", R, top);getchar();
   *++rp = top, pop;
   //printf(">R R=%p *R=0x%X", R, *R);getchar();
 ; NEXT
/* R> */ case 0x069:          
   //printf("R> R=%p *R=0x%X", R, *R);getchar();
   push *rp--; 
   //printf("R> R=%p top=0x%X", R, top);getchar();
 ; NEXT
/* R@ */ case 0x06A:          push *rp  ; NEXT
/* I */ case 0x06B:           push R[0] + R[-1]; NEXT
/* J */ case 0x06C:           push R[-3] + R[-4]; NEXT
/* LEAVE */ case 0x06D:       I = (byte*)R[-2];
/* UNLOOP */ case 0x06E:      R -= 3; NEXT
/* INVERT */ case 0x070:   top = ~top; NEXT
/* NEGATE */ case 0x071:   top = -top; NEXT
#define LOWER(u1,u2)  ((uint32_t)(u1) < (uint32_t)(u2))
/* WITHIN */ case 0x072:  
  w = *S--,
  top = LOWER(*S - w, top - w) LOGICAL;
  S--;
  NEXT
/* M* */ case 0x073:   {
  int64_t d = (int64_t)*S * (int64_t)top;
  *S = d ;
  top = d >> 32;
  NEXT }
/* UM* */ case 0x074:   {
  uint64_t u1 = (uint32_t)*sp;
  uint64_t u2 = (uint32_t)top;
  uint64_t ud = u1 * u2;
  *sp = ud ;
  top = ud >> 32;
  NEXT }
/* UM/MOD */ case 0x075:   {
  uint64_t ud = ((uint64_t)*S << 32) | (uint32_t)S[-1];
  uint64_t u = (uint32_t)top;
  uint32_t quot = ud / u;
  uint32_t rem = ud % u;
  *--S = rem;
  top = quot;
  NEXT }
/* SM/REM */ case 0x076:   {
  int64_t d = (((uint64_t)*S) << 32) | ((uint32_t) S[-1]);
  int32_t quot = d / top;
  int32_t rem = d % top;
  *--S = rem;
  top = quot;
  NEXT }
/* CELLS */ case 0x077:       top *= CELL; NEXT
/* C@ */ case 0x078:    top = m[top]; NEXT
/* C! */ case 0x079:    m[top] = *sp; pop2; NEXT
/* MOVE */ case 0x07A:   memmove(phys(*S), phys(S[-1]), top); pop3; NEXT
/* PHYS */ case 0x07B:   top += (cell)m; NEXT   // convert to physical address
/* VIRT */ case 0x07C:   top -= (cell)m; NEXT   // convert to absolute address
/* KEY */ case 0x07D:    push getchar(); NEXT
/* EMIT */ case 0x07E:    putchar(top); pop; NEXT
/* TYPE */ case 0x07F:     type(*sp, top); pop2; NEXT
/* BYE */ case 0x080:   return;
/* ACCEPT */ case 0x081:    top = accept(*sp--, top);
/* FIXME */ if (top < 0) exit(0); NEXT
/* ALLOCATE */ case 0x082:    *++S = virt(malloc(top)), top = *S ? 0 : -1; NEXT
/* RESIZE */ case 0x083:      *S = virt(realloc(phys(*S), top)), top = *S ? 0 : -1; NEXT
/* FREE */ case 0x084:        free(phys(top)), top = 0; NEXT
/* NEW-STRING */ case 0x085:  top = new_string(*sp--, top); NEXT
/* OPEN-FILE */ case 0x086:   {
  //printf("open a=0x%x, u=%d, fam=%d\n", S[-1], *S, top);
  char *filename = phys(new_string(S[-1], *S));
  FILE *file = fopen(filename, "r");
  //printf("open %s returned %p\n", filename, file);
  free(filename);
  *--S = (cell) file;
  top = file ? 0 : -1; NEXT }
/* CLOSE-FILE */ case 0x087:    
  //printf("closing %p\n", (FILE*)top);
  top = fclose((FILE*)top); NEXT
/* REFILL */ case 0x088:    push refill(SOURCE); NEXT
/* . */ case 0x089:    printf("%d ", top); pop; NEXT
/* -NUMBER */ case 0x08A:   w = number(top, ++sp);
  if (w) top = 0; else *sp = top, top = -1; NEXT
/* WORD */ case 0x08B:  
  top = word(SOURCE, top, HERE); NEXT
/* FIND */ case 0x08C:  
      w = find(top, 1);
      if (w) *++sp = cfa(w), top = -1;
      else push 0; NEXT
/* -FIND */ case 0x08D:  
      w = find(*sp, top);
      if (w) *sp = cfa(w), top = 0;
      else top = -1; NEXT
/* DEPTH */ case 0x08E:    w = sp - stack; push w; NEXT
/* .S */ case 0x08F:  
      w = sp - stack;  sp[1] = top;
      printf("[%d] ", w);
      for (int i = 0; i < w; i++)
          printf("%d (0x%x) ", stack[i+2], stack[i+2]);
      NEXT
/* WORDS */ case 0x090:    words(M(CONTEXT)); NEXT
/* DUMP */ case 0x091:    dump(*sp--, top); pop; NEXT
/* LIMIT */ case 0x092:   push sizeof m; NEXT
/* ALIGNED */ case 0x093:     top = aligned(top); NEXT
/* PARSE */ case 0x094:    top = parse(SOURCE, top, ++S); NEXT
