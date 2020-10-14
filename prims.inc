#define S sp
#define R rp
#define I ip
case 0x000:  /* EXIT */  xit:  ip = (opcode*) *rp--; NEXT
/* EXECUTEX */ case 0x001:        w = top; pop; goto exec;
case 0x002:  /* BRANCH */    ip += *(cell*)ip; NEXT
case 0x003:  /* DO */        ; NEXT // TODO
case 0x004:  /* ?DO */       ; NEXT // TODO
case 0x005:  /* LOOP */      ; NEXT // TODO
case 0x006:  /* +LOOP */     ; NEXT // TODO
case 0x007:  /* DLIT */      push *ip++; push *ip++; NEXT
case 0x008:  /* DOVAR */     push (uchar*)ip++ - m; goto xit;
case 0x009:  /* ." */        ip = dotq(ip); NEXT
case 0x00A:  /* S" */        push virt(ip) + 1; push *ip; ip = litq(ip); NEXT
case 0x00B:  /* abort" */    if (top) {
  dotq(m + HERE); putchar(BL); dotq(ip);
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
case 0x058:    ip += top ? CELL : *(cell*)ip; pop; NEXT
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
/* CELLS */ case 0x070:       top *= CELL; NEXT
/* C@ */ case 0x071:    top = m[top]; NEXT
/* C! */ case 0x072:    m[top] = *sp; pop2; NEXT
/* PHYS */ case 0x073:   top += (cell)m; NEXT   // convert to physical address
/* VIRT */ case 0x074:   top -= (cell)m; NEXT   // convert to absolute address
/* KEY */ case 0x075:    push getchar(); NEXT
/* EMIT */ case 0x076:    putchar(top); pop; NEXT
/* TYPE */ case 0x077:     type(*sp, top); pop2; NEXT
/* . */ case 0x078:    printf("%d ", top); pop; NEXT
/* H. */ case 0x079:    printf("0x%X ", top); pop; NEXT
/* BYE */ case 0x07A:   return;
/* ACCEPT */ case 0x07B:    top = accept(*sp--, top);
/* FIXME */ if (top < 0) exit(0); NEXT
#define SOURCE M(8)
/* ALLOCATE */ case 0x07C:    *++S = virt(malloc(top)), top = *S ? 0 : -1; NEXT
/* RESIZE */ case 0x07D:      *S = virt(realloc(phys(*S), top)), top = *S ? 0 : -1; NEXT
/* FREE */ case 0x07E:        free(phys(top)), top = 0; NEXT
/* NEW-STRING */ case 0x07F:  top = new_string(*sp--, top); NEXT
/* OPEN-FILE */ case 0x080:   {
  //printf("open a=0x%x, u=%d, fam=%d\n", S[-1], *S, top);
  char *filename = phys(new_string(S[-1], *S));
  FILE *file = fopen(filename, "r");
  printf("open %s returned %p\n", filename, file);
  free(filename);
  *--S = (cell) file;
  top = file ? 0 : -1; NEXT }
/* CLOSE-FILE */ case 0x081:    
  printf("closing %p\n", (FILE*)top);
  top = fclose((FILE*)top); NEXT
/* REFILL */ case 0x082:    push refill(SOURCE); NEXT
/* -NUMBER */ case 0x083:   w = number(top, ++sp);
  if (w) top = 0; else *sp = top, top = -1; NEXT
/* WORD */ case 0x084:  
  top = word(SOURCE, top, HERE); NEXT
/* FIND */ case 0x085:  
      w = find(top, 1);
      if (w) *++sp = cfa(w), top = -1;
      else push 0; NEXT
/* -FIND */ case 0x086:  
      w = find(*sp, top);
      if (w) *sp = cfa(w), top = 0;
      else top = -1; NEXT
/* DEPTH */ case 0x087:    w = sp - stack; push w; NEXT
/* .S */ case 0x088:  
      w = sp - stack;  sp[1] = top;
      printf("[%d] ", w);
      for (int i = 0; i < w; i++)
          printf("%d (0x%x) ", stack[i+2], stack[i+2]);
    ; NEXT
/* WORDS */ case 0x089:    words(M(CONTEXT)); NEXT
/* DUMP */ case 0x08A:    dump(*sp--, top); pop; NEXT
/* ALIGNED */ case 0x08B:     top = aligned(top); NEXT
/* PARSE */ case 0x08C:    top = parse(SOURCE, top, ++S); NEXT
