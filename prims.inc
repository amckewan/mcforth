case 0x000:  /* EXIT */	xit:  ip = (cell*) *rp--; NEXT
/* EXECUTE */ case 0x001:  	w = top; pop; goto exec;
case 0x002:  /* BRANCH */	ip += *ip; NEXT
case 0x003:  /* DO */		NEXT
case 0x004:  /* ?DO */		NEXT
case 0x005:  /* LOOP */		NEXT
case 0x006:  /* +LOOP */		NEXT
case 0x007:  /* DLIT */		push *ip++; push *ip++; NEXT
case 0x008:  /* DOVAR */		push (uchar*)ip++ - m; goto xit;
case 0x009:  /* ." */		ip = dotq(ip); NEXT
case 0x00A:  /* S" */		push (cell)ip + 1; push *(uchar*)ip; ip = litq(ip); NEXT
case 0x00B:  /* abort" */	if (top) { dotq((cell*)(m + HERE)); putchar(BL); dotq(ip); goto abort; }
ip = litq(ip); pop; NEXT
/* + */ case 0x010:  		top += *sp--; NEXT
/* - */ case 0x011:  		top = *sp-- - top; NEXT
/* AND */ case 0x012:  	top &= *sp--; NEXT
/* OR */ case 0x013:  		top |= *sp--; NEXT
/* XOR */ case 0x014:  	top ^= *sp--; NEXT
/* LSHIFT */ case 0x015:  	top = *sp-- << top; NEXT
/* RSHIFT */ case 0x016:  	top = (cell) ((ucell)(*sp--) >> top); NEXT
/* ARSHIFT */ case 0x017:  top = *sp-- >> top; NEXT
/* SWAP */ case 0x018:  	w = top; top = *sp; *sp = w; NEXT
/* PICK */ case 0x019:  	top = sp[-top]; NEXT
/* @ */ case 0x01A:  		top = M(top); NEXT
/* ! */ case 0x01B:  		M(top) = *sp; pop2; NEXT
/* +! */ case 0x01C:  		M(top) += *sp; pop2; NEXT
/* * */ case 0x01D:  		top *= *sp--; NEXT
/* / */ case 0x01E:  		top = *sp-- / top; NEXT
/* NOP */ case 0x01F:  	NEXT
case 0x020:  	top += *ip++; NEXT
case 0x021:  	top -= *ip++; NEXT
case 0x022:  	top &= *ip++; NEXT
case 0x02F:  	push *ip++; NEXT
/* 0= */ case 0x040:  		top = (top == 0) LOGICAL; NEXT
/* 0< */ case 0x041:  		top = (top < 0) LOGICAL; NEXT
/* 0> */ case 0x042:  		top = (top > 0) LOGICAL; NEXT
/* = */ case 0x043:  		top = (*sp-- == top) LOGICAL; NEXT
/* < */ case 0x044:  		top = (*sp-- < top) LOGICAL; NEXT
/* > */ case 0x045:  		top = (*sp-- > top) LOGICAL; NEXT
/* U< */ case 0x046:  		top = ((ucell)*sp-- < (ucell)top) LOGICAL; NEXT
/* U> */ case 0x047:  		top = ((ucell)*sp-- > (ucell)top) LOGICAL; NEXT
case 0x048:  		top = (top != 0) LOGICAL; NEXT
case 0x049:  		top = (top >= 0) LOGICAL; NEXT
case 0x04A:  		top = (top <= 0) LOGICAL; NEXT
case 0x04B:  		top = (*sp-- != top) LOGICAL; NEXT
case 0x04C:  		top = (*sp-- >= top) LOGICAL; NEXT
case 0x04D:  		top = (*sp-- <= top) LOGICAL; NEXT
case 0x04E:  		top = ((ucell)*sp-- >= (ucell)top) LOGICAL; NEXT
case 0x04F:  		top = ((ucell)*sp-- <= (ucell)top) LOGICAL; NEXT
case 0x058:  	ip += top ? 1 : *ip; pop; NEXT
/* DROP */ case 0x060:  	pop; NEXT
/* DUP */ case 0x061:  	*++sp = top; NEXT
/* ?DUP */ case 0x062:  	if (top) *++sp = top; NEXT
case 0x063:  	NEXT
/* OVER */ case 0x064:  	NEXT
/* C@ */ case 0x065:    top = m[top];  NEXT
/* C! */ case 0x066:    m[top] = *sp; pop2; NEXT
/* KEY */ case 0x067:    push getchar(); NEXT
/* EMIT */ case 0x068:    putchar(top); pop; NEXT
/* TYPE */ case 0x069:     type(*sp, top); pop2; NEXT
/* CR */ case 0x06A:         putchar('\n'); NEXT
/* ACCEPT */ case 0x06B:    top = accept(*sp--, top); NEXT
/* +M */ case 0x06C:   top += (cell)m; NEXT	// convert to physical address
/* -M */ case 0x06D:   top -= (cell)m; NEXT	// convert to absolute address
/* -NUMBER */ case 0x06E:   w = number(top, ++sp);
if (w) top = 0; else *sp = top, top = -1; NEXT
/* WORD */ case 0x06F:    top = word(top, (Input*)(m+8), m+HERE) - m; NEXT
/* FIND */ case 0x070:  
	w = find(top, 1);
	if (w) *++sp = cfa(w), top = -1;
	else push 0; NEXT
/* -FIND */ case 0x071:  
	w = find(*sp, top);
	if (w) *sp = cfa(w), top = 0;
	else top = -1; NEXT
/* . */ case 0x072:    printf("%d ", top); pop; NEXT
/* DEPTH */ case 0x073:  	 w = sp - stack; push w; NEXT
/* .S */ case 0x074:  
      w = sp - stack;  sp[1] = top;
      printf("[%d] ", w);
      for (int i = 0; i < w; i++)
          printf("%d ", stack[i+2]);
      NEXT
/* WORDS */ case 0x075:    words(M(CONTEXT)); NEXT
/* DUMP */ case 0x076:    dump(*sp--, top); pop; NEXT
/* BYE */ case 0x077:   return;
/* ALIGNED */ case 0x078:  top = aligned(top); NEXT
/* CELLS */ case 0x079:  	top *= CELL; NEXT
