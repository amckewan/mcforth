include extend.fs

: hi ." Hello, shall we test?" CR ;
hi

: x 10 0 do i . loop ;
: y 0 10 do i . -1 +loop ;
: z 10 10 ?do ." oops " loop ;

: xx 3 0 do  cr 5 0 do j . i . loop loop ;
: yy 10 0 do i . i 5 = if leave ." oops " then loop ;
: zz 10 0 do i . i 5 = if unloop exit then loop ." oops " ;

\ include test/tester.fs

CR .( testing finished! )
