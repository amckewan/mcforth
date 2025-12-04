( decompiler )
( intended to see what the compiler does, not to reproduce source code )

need module from lib/module.f

base @ hex

\ cheesy decompiler
: cee ' >name 20 dump ;

internal

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

: .call ( a -- a+2 ) dup w@ cells ( dup . ) .name 2 + ;
: .callx ( a -- a+4 ) dup @ ( dup . ) .name cell+ ;
: .branch ( a -- a+1 ) dup dup c@ dup 80 and if -1 ff xor or then + . 1+ ;
: .lit  dup ? cell + ;
: .slit  count 2dup type '"' emit space + ;

: .operands ( a opc -- a' )
    \ case?
    dup 1 = over 9 = or if drop .call exit then
    dup 2 = if drop .callx exit then
    dup 3 8 within over D = or over 50 60 within or if drop .branch exit then
    dup 20 40 within if drop .lit exit then
    dup 40 50 within if drop .lit .branch exit then
    dup A D within if drop .slit exit then
    dup 18 = if drop count . count . exit then
    dup 1A 1C within if drop count . exit then
    drop ;

external

: (see) ( xt -- )
    \ dup >name count 1f and type
    begin
        cr dup .  dup count ( a a+1 op )
        dup .b dup .op .operands ( a a+1 )
        over c@ 12 = if  drop @ 8 rshift 0
        else swap c@  dup 0=  over 9 = or  swap 10 18 within or  then
    until drop ;

: see ' (see) ;

: see:  here ] 1 parse evaluate postpone exit postpone [ (see) ;

module
base !
