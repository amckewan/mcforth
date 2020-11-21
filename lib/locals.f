\ Local Variables
\ Copyright (c) 2020 Andrew McKewan

forth decimal

variable h'  \ a place to store local names
: teleport  h @ h' dup @ h ! ! ;

\ Locals are added to the locals vocabulary (6)
\ when executed they compile code to fetch the local

: locals  6 context ! ;

variable #params    \ # of named parameters
variable #locals    \ # of locals defined so far (including params)

: L, ( l# op -- )  op,  #locals @ swap - c, ;

: make-local
    create  #locals @ ,  1 #locals +!
    does> @  $1A ( L@ ) L, ;

: local  context @ locals  teleport make-local teleport  context ! ;
: param  local  1 #params +! ;

: locals,       $18 op, ( L{ )  #params @ #locals @ over - c, c, ;
: end-locals,   #locals @ if  $19 op, ( }L )  then ;

: clear-locals  [ context 6 cells + dup @ ] literal literal ! ;
: init-locals
    clear-locals
    #params off  #locals off
    here 1000 + h' ! ;

: :         init-locals : ;
: :noname   init-locals :noname ;

compiler
: -->   6 -' abort" local?"  >body @  $1B ( L! ) L, ;
: does> end-locals,  postpone does>  init-locals ;
: exit  end-locals,  \\ exit  ;
: ;     \\ exit  \\ [  reveal ( oh no! )  clear-locals ;

\ syntax { param1 param2 | local1 local2 -- results }
: { 0 begin
        >in @ char swap >in !
        dup 0=  over '-' = or  over '}' = or  not
    while
        '|' = if  char or
        else  dup if local else param then  then
    repeat
    2drop '}' parse 2drop
    #locals @ if locals, then ;
forth

\ test

: t [ local a local b locals, ] a . b . ;

: add [ param a param b locals, ] a b + ;

: add1 { a b } a b + ;
: add2 { a b -- c } a b + ;
: add3 { a b | c -- c } a b + --> c  c ;

