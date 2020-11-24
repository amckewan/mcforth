\ path for include

\ : INCLUDED  ( str len -- )
\     2DUP R/O OPEN-FILE ABORT" file not found"
\     >SOURCE  BEGIN REFILL WHILE INTERPRET REPEAT  SOURCE> ;

: ./?  ( a n -- a' n' f )
    file @ file? if
        over 2 s" ./" compare 0= if
            SOURCE-NAME


    then ;

: add-path ( str len -- str' len' )

;

: included  ( str len -- )
    add-path over >r
    included
    r> free drop
;

: include  parse-name included ;
