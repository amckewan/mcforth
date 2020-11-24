\ path for include

need string from lib/string.f

\ : INCLUDED  ( str len -- )
\     2DUP R/O OPEN-FILE ABORT" file not found"
\     >SOURCE  BEGIN REFILL WHILE INTERPRET REPEAT  SOURCE> ;

: ./?  ( a n -- a' n' f )
    source-file @ file? not if 0 exit then

    over 2 s" ./" compare 0= over 1 > and if


    then ;

: add-path ( str len -- str' len' )
    \  this would be nice :) { string s }

    \ if the path starts with "./", and we are including from another file, use the same base path
    s" ./" path starts-with if
        souce-name @ count 0 2 path replace ( addr len offset len obj -- )


    string new dup>r put
    s" ." r@ starts-with


    r> release ;
;

: included  ( str len -- )
    add-path over >r
    included
    r> free drop
;

: include  parse-name included ;
