\ Local Variables
\ Copyright (c) 2020 Andrew McKewan

forth decimal

variable h'  \ space to store local names
: flip  h @ h' dup @ h ! ! ;

\ Locals are added to the locals vocabulary (6)
\ when executed they compile code to fetch the local

: locals  6 context ! ;

variable #params    \ # of named parameters
variable #locals    \ # of locals defined so far (including params)

: L, ( l# op -- )  op,  #locals @ swap - c, ;

: make-local
    create  #locals @ ,  1 #locals +!
    does> @  $1A ( L@ ) L, ;

: local  context @ locals  flip make-local flip  context ! ;
: param  local  1 #params +! ;

: locals,   $18 op, ( L{ )  #params @ #locals @ over - c, c, ;

: clear-locals
    #params off  #locals off
    [ context 6 cells + dup @ ] literal literal !
    here 1000 + h' ! ;

: :     clear-locals  :  ;
compiler
: -->   6 -' abort" local?"  >body @  $1B ( L! ) L, ;
: exit  #locals @ if  $19 op, ( }L )  then  \\ exit  ;
: ;     \\ exit  \\ [  reveal  clear-locals ( needed?) ;
forth

\ test

: t [ local a local b locals, ] a . b . ;

: add [ param a param b locals, ] a b + ;
