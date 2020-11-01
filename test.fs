.( Hello, shall we test?) CR

: x 10 0 do i . loop ;
: y 0 10 do i . -1 +loop ;
: z 10 10 ?do ." oops " loop ;

: xx 3 0 do  cr 5 0 do j . i . loop loop ;
: yy 10 0 do i . i 5 = if leave ." oops " then loop ;
: zz 10 0 do i . i 5 = if unloop exit then loop ." oops " ;

include test/tester.fs
\ VERBOSE ON
include ../forth2012-test-suite/src/core.fr
include test/strings.fs




decimal
CR .( Testing finished: ) .TESTS
