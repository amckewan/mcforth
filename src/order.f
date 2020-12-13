( search order )

create context  9 cells allot  context 9 cells erase

variable vocs
: wordlist  align here 0 , vocs link, ;
: vocabulary  create wordlist drop  does> context ! ;

wordlist constant only-wordlist

: get-order  ( -- wid1..widn n )
    0  0 7 do  i cells context + @ ?dup if swap 1+ then  -1 +loop ;

: set-order  ( wid1..widn n -- )
    context 8 cells erase
    dup 0< if drop only-wordlist 1 then
    0 ?do  i cells context + !  loop ;

: get-current  current @ ;
: set-current  current ! ;
: definitions  context @ current ! ;

: .id  ( xt -- )  >name count 31 and type space ;

: .voc  ( take a guess )
    dup forth-wordlist = if ." forth " drop exit then
    dup only-wordlist  = if ." only "  drop exit then
    dup cell - c@ $12 = if cell - .id else . then ;

: order
    ." Context: " get-order 0 ?do .voc loop cr
    ." Current: " current @ .voc cr ;

: only      -1 set-order ;
: forth     forth-wordlist context ! ;
: also      context dup cell+ 7 cells move ;
: previous  context cell+ context 8 cells move ;

only forth also definitions
