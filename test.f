.( Hello, shall we test?) CR

\ exit on error
: BYE CR BYE ; ' BYE 1 CELLS !

include lib/standard.f

\ include ../forth2012-test-suite/src/prelimtest.fth

include test/tester.f
\ VERBOSE ON
include test/value.f
include test/strings.f

\ fm/mod fails, accept removed
include ../forth2012-test-suite/src/core.fr

\ missing stuff
\ include ../forth2012-test-suite/src/stringtest.fth

\ gforth tests
\ include /usr/share/gforth/0.7.3/test/
\ include /usr/share/gforth/0.7.3/test/coretest.fs
include /usr/share/gforth/0.7.3/test/postpone.fs

decimal
CR .( Testing finished: ) .TESTS
