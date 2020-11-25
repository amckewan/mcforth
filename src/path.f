\ path for include

: find-last-/  ( a n -- a n' )
    begin dup while
        1- 2dup + c@ '/' = if  1+ exit  then
    repeat ;

\ return the path up to and including the last /
\ returns ./ if no / in the path
: dirname ( a n -- a n' )
    find-last-/  dup 0= if  2drop s" ./"  then ;

\ returns the filename after the last / (which may be empty)
\ return original input if no /
: basename ( a n -- a' n' )
    2dup find-last-/  nip /string ;


\ true if filename starts with "./"
: ./?  ( a n -- a n f )
    over 2 s" ./" compare 0=  over 1 > and ;

\ append ./basename to dirname of the current source file
: append-to-dir ( a n -- a' n' )
    source-name @ count dirname
    >r pad r@ move ( a n )
    2 /string  pad r@ + swap  dup>r move
    pad r> r> + ;

\ If the file starts with "./", try to include it from
\ the same directory as the current source file.
: open-on-path ( a n -- a' n' fid )
    ./? source-file @ file? and if
        2dup append-to-dir
        2dup r/o open-file 0= if  >r rot drop rot drop r> exit  then  drop
    then
    \ todo: search path goes here
    2dup r/o open-file abort" file not found" ;

: included  ( str len -- )
    open-on-path >source
    begin refill while interpret repeat
    source> ;

: include  parse-name included ;
