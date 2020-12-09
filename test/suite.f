S" Testing 1,2,3..." TYPE CR

( exit on error )
: BYE CR BYE ; ' BYE 1 CELLS !

include lib/standard.f

include test/tester.f
VERBOSE ON
: FAILED \\ \ ; ( to mark tests that fail )

include test/core.fr
include test/coreexttest.fth
include test/coreplustest.fth
include test/value.f
include test/strings.f
include test/stringtest.fth
\ include test/pathtest.f

decimal
CR S" Testing finished: " TYPE .TESTS
