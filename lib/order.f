( search order )

\ create context  9 cells allot  context 9 cells erase

create voc-link  forth-wordlist cell+ ,

: wordlist  align  here 0 ,  voc-link link, ;
: vocabulary  create wordlist drop  does> context ! ;

: forth     forth-wordlist context ! ;
: only      context 8 cells erase  forth ;
: also      context dup cell+ 7 cells move ;
: previous  context cell+ context 8 cells move ;

: definitions  context @ current ! ;

only forth also definitions

need .name : .name  ( nfa -- )  count 31 and type space ;

: .voc  ( take a guess )
    dup forth-wordlist = if ." FORTH " drop exit then
    dup cell - c@ $12 = if cell - >name .name else . then ;

: vocs  voc-link @ begin  dup cell - .voc  @ ?dup 0= until ;

: order
    ." Context: " context begin dup @ while dup @ .voc cell+ repeat drop cr
    ." Current: " current @ .voc cr ;

vocabulary hidden

\S
\ standard words not included

: get-order  ( -- wid1..widn n )
    0  0 7 do  i cells context + @ ?dup if swap 1+ then  -1 +loop ;

: set-order  ( wid1..widn n -- )
    context 8 cells erase
    dup 0< if drop forth-wordlist 1 then
    0 ?do  i cells context + !  loop ;

: get-current  current @ ;
: set-current  current ! ;

