.( Testing 1,2,3... ) CR

\ exit on error
: BYE CR BYE ; ' BYE 1 CELLS !

\ need standard stuff
include opt/standard.f

include test/tester.f
VERBOSE ON
: FAILED \\ \ ; ( to mark tests that fail )

include test/core.fr
include test/coreexttest.fth
include test/coreplustest.fth
include test/value.f
include test/strings.f
include test/stringtest.fth
include test/localstest.f

\ gforth tests
\ include /usr/share/gforth/0.7.3/test/
\ include /usr/share/gforth/0.7.3/test/coretest.fs
\ include /usr/share/gforth/0.7.3/test/postpone.fs

decimal
CR .( Testing finished: ) .TESTS
