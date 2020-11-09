( decompiler )
( intended to see what the compiler does, not to reproduce source code )

base @ hex

\ cheesy decompiler
: cee ' >name 20 dump ;

\ build opcode table
create ops 100 cells allot
       ops 100 cells erase

: op ( op <name> )  here swap cells ops + !  bl word c@ 1+ allot ;

include see.info

: op? ( op -- c-str t | 0 ) cells ops + @ ?dup ;

: .op ( op )  op? if count type else ." ---" then space ;

: .ops  100 0 do  i op? if i . count type space then  loop ;

: .b  base @ hex  swap 0 <# # # #> type space  base ! ;
: .name ( xt -- ) >name count 1f and type space ;

: .call ( a -- a+2 ) dup w@ ( dup . ) .name 2 + ;
: .call32 ( a -- a+4 ) dup @ ( dup . ) .name cell+ ;
: .branch ( a -- a+1 ) dup dup c@ dup 80 and if -1 ff xor or then + . 1+ ;
: .lit  dup ? cell + ;
: .slit  count 2dup type '"' emit space + ;

: .operands ( a opc -- a' )
    \ case?
    dup 1 = if drop .call exit then
    dup 2 = if drop .call32 exit then
    dup 3 8 within over 50 60 within or if drop .branch exit then
    dup 8 = over 20 40 within or if drop .lit exit then
    dup 40 50 within if drop .lit .branch exit then
    dup A D within if drop .slit exit then
    drop ;

: (see) ( xt -- )
    \ dup >name count 1f and type
    begin
        dup . dup count ( a a+1 op )
        dup .b dup .op .operands cr
        swap c@ dup 0= swap 10 20 within or
    until drop ;

: see ' (see) ;

: see:  here ] 1 parse evaluate \\ exit \\ [ (see) ;

: s see: ; \ shorthand

base !