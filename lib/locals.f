\ Local Variables
\ Copyright (c) 2020 Andrew McKewan

forth decimal

\ Local variables are added to the locals vocabulary (6)
\ when executed they compile code to fetch the local

: locals  6 context ! ;

internal

variable h'  \ a place to store local names
: bounce  h @ h' dup @ h ! ! ;

variable #params    \ # of named parameters
variable #locals    \ # of locals defined (including params)

: L, ( l# op -- )  op,  #locals @ swap - c, ;

: make-local
    create  #locals @ ,  1 #locals +!
    does> @  $1A L, ( L@ ) ;

: local  context @ locals bounce  make-local  bounce context ! ;
: param  local  1 #params +! ;

: locals,     $18 op, ( L{ )  #params @ #locals @ over - c, c, ;
: end-locals  #locals @ if  $19 op, ( }L )  then ;

: init-locals
    [ context 6 cells + dup @ ] literal literal !
    h @ 1000 + h' !
    0 #params !  0 #locals ! ;

external

: :         init-locals : ;
: :noname   init-locals :noname ;

\ todo: patch this into methods definitions, who goes first?
\ perhaps methods depend on locals

compiler
: to    >in @  6 -' if  drop >in !  \\ to
                  else  >body @  $1B L, ( L! )  drop then ;
: does> end-locals  postpone does>  init-locals ;
: exit  end-locals  \\ exit  ;
: ;     end-locals  \\ ;  ;

\ syntax { param1 param2 | local1 local2 -- results }
: { 0 begin
        >in @ char swap >in !
        dup '-' =  over '}' = or  over 0= or  not
    while
        '|' = if  char or
        else  dup if local else param then  then
    repeat
    2drop '}' parse 2drop
    #locals @ if locals, then ;
forth

module
