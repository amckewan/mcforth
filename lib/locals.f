\ Local Variables
\ Copyright (c) 2020 Andrew McKewan

need order from src/order.f
need module from src/module.f

forth decimal

\ Local variables are added to the locals wordlist which is
\ temporarily added to the top of the search order.
\ They are immediate words that compile code to fetch the local.

variable locals

internal

variable h'  \ a place to store local names
: bounce  h @ h' dup @ h ! ! ;

variable #params    \ # of named parameters
variable #locals    \ # of locals defined (including params)

: locals,   locals @ if  $18 op, ( L{ )  #params @ #locals @ over - c, c,  then ;
: unlocal   locals @ if  $19 op, ( }L )  then ;

: init-locals ( -- )
    here 1000 + h' !
    also locals context !
    0 #params !  0 #locals ! ;

: end-locals  context @ locals = if previous then  unlocal ;

: L, ( l# op -- )  op,  #locals @ swap - c, ;

: make-local ( addr len -- )
    locals (header) immediate  cell allot  #locals @ ,  1 #locals +!
    does> @  $1A L, ( L@ ) ;

external

: (local)  ( name len -- )
    ?dup 0= if  drop locals, exit  then
    locals @ 0= if  init-locals  then
    last 2@ 2swap  bounce make-local bounce  last 2! ;

: local  parse-name (local) ;
: param  local  1 #params +! ;

: to  ' dup here > if  >body @ $1B L, ( L! ) else (to) then ; immediate

: :         :        0 locals ! ;
: :noname   :noname  0 locals ! ;

: exit      unlocal     postpone exit  ; immediate
: does>     end-locals  postpone does>  init-locals ; immediate
: ;         end-locals  postpone ;     ; immediate

\ syntax { param1 param2 | local1 local2 -- results }
: { 0 begin
        >in @ char swap >in !
        dup '-' =  over '}' = or  over 0= or  not
    while
        '|' = if  char or
        else  dup if local else param then  then
    repeat
    2drop '}' parse 2drop
    locals, ; immediate

module

\ standard local extension words
\S TODO

: locals| ;
: {: ;

\ module
