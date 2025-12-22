( curses )

0 value libcurses

: ?dlerror ( f -- )
    if  dlerror sbuf place  sbuf msg !  -2 throw  then ;

: open-curses  ( -- f )
    s" libncurses.so.6" dlopen  dup 0= ?dlerror  to libcurses ;

: close-curses ( -- )
    libcurses ?dup if  dlclose  0 to libcurses  then ;

: getsym ( addr len -- sym )
    libcurses dlsym  dup 0= ?dlerror ;

: call parse-name getsym postpone literal postpone dlcall ; immediate

open-curses
s" initscr" getsym constant 'initscr

: initscr 0 'initscr dlcall drop ;
: endwin 0 [ s" endwin" getsym ] literal dlcall drop ;
: refresh 0 call refresh drop ;

\ global variables are actually functions with an _nc_ prefix
\ no, this is only if compiled with NCURSES_REENTRANT
\ : stdscr  0 call _nc_stdscr ;
s" stdscr" getsym >rel constant 'stdscr
: stdscr 'stdscr @ ;

: var  parse-name getsym >rel  postpone literal ; immediate

: stdscr var stdscr @ ;

: LINES  var LINES @ ;
: COLS   var COLS @ ;

\ many of these are marcros not functions, so we have to cheat
: waddch ( win c -- )  2 call waddch drop ;
: addch ( c -- )  stdscr swap waddch ;

: mv  ( y x -- )  2 call move drop ;

: addstr ( addr len -- )  >r stdscr swap >abs r> 3 call waddnstr drop ;

\  constant mvaddnstr(y,x,str,n)        mvwaddnstr(stdscr,(y),(x),(str),(n))
: mvwaddstr ( y x str len -- )
    swap >abs swap 5 call mvwaddnstr drop ;
: mvaddstr ( y x str len -- )
    >r >r >r >r stdscr r> r> r> r> mvwaddstr ;

( Attributes )
: attr ( n -- attr )  1 swap 8 + lshift constant ;

 0 constant A_NORMAL
 8 attr A_STANDOUT
 9 attr A_UNDERLINE
10 attr A_REVERSE
11 attr A_BLINK
12 attr A_DIM
13 attr A_BOLD
23 attr A_ITALICS

: color-pair ( n -- mask )  255 and 8 lshift ;

: wattron  ( win attr -- )  2 call wattron drop ;
: wattroff ( win attr -- )  2 call wattroff drop ;
: wattrset ( win attr -- )  2 call wattrset drop ;

: attron  ( attr -- )  stdscr swap wattron ;
: attroff ( attr -- )  stdscr swap wattroff ;
: attrset ( attr -- )  stdscr swap wattrset ;

( Colors )
0 constant COLOR_BLACK
1 constant COLOR_RED
2 constant COLOR_GREEN
3 constant COLOR_YELLOW
4 constant COLOR_BLUE
5 constant COLOR_MAGENTA
6 constant COLOR_CYAN
7 constant COLOR_WHITE

: start-color  0 call start_color drop ;
: init-color  ( color r g b -- )  4 call init_color drop ;
: init-pair  ( pair fg bg -- )  3 call init_pair drop ;

( Line graphics )
\ acs_map isn't filled in until after initscr so we can't
\ define these at build time
s" acs_map" getsym >rel constant acs_map
: acs  ( c -- x )  cells acs_map + @ ;
: acs: ( c -- )  create , does> @ acs ;

char 0 acs: ACS_BLOCK
char q acs: ACS_HLINE
char x acs: ACS_VLINE
char l acs: ACS_ULCORNER
char k acs: ACS_URCORNER
char m acs: ACS_LLCORNER
char j acs: ACS_LRCORNER
char t acs: ACS_LTEE
char u acs: ACS_RTEE
char w acs: ACS_TTEE
char v acs: ACS_BTEE
char n acs: ACS_PLUS

\ or do it by hand
: acs-emit ( c -- )  acs addch ;
: acs-type ( str len -- )  0 ?do count acs-emit loop drop ;


\ ************** TESTING ****************

: acs-test
    s" hv~a.f`>qi,ymjt|{gn+uoprs}w-lkx" 2dup
    2 0 mv 0 do count addch bl addch loop drop
    3 0 mv 0 do count acs-emit bl addch loop drop ;

\ 'z' 1+ 'a' do i acs-emit loop ;



: box
    10 5 mv ACS_ULCORNER addch ACS_HLINE addch ACS_URCORNER addch
    11 5 mv ACS_VLINE addch 'X' addch ACS_VLINE addch
    12 5 mv ACS_LLCORNER addch ACS_HLINE addch ACS_LRCORNER addch
;

: box2
    10 20 mv ACS_ULCORNER addch ACS_HLINE addch ACS_HLINE addch ACS_HLINE addch ACS_URCORNER addch
    11 20 mv ACS_VLINE addch bl addch bl addch bl addch ACS_VLINE addch
    12 20 mv ACS_LLCORNER addch ACS_HLINE addch ACS_HLINE addch ACS_HLINE addch ACS_LRCORNER addch
;

: bigbox
    10 10 mv s" lqwqk" acs-type
    11 10 mv s" x~x~x" acs-type
    12 10 mv s" tqnqu" acs-type
    13 10 mv s" x~x~x" acs-type
    14 10 mv s" mqvqj" acs-type
;

: +B A_BOLD attron ;
: -B A_BOLD attroff ;
: +I A_ITALICS attron ;
: -I A_ITALICS attroff ;


: row  ( n -- )
    dup 2* 21 + 0 mv
    \ even rows start with white square (arbitrary)
    'x' acs-emit  8 0 do
        \ 1 xor dup 1 and if COLOR_WHITE else COLOR_BLACK then
        bl
        dup dup addch addch addch
        'x' acs-emit
    loop drop ;

: break ( n -- )
    2* 22 + 0 mv
    s" tqqqnqqqnqqqnqqqnqqqnqqqnqqqnqqqu" acs-type ;

: piece ( char row col -- )
    swap 2* 21 + swap 4 * 2 + mv addch ;

: pieces
    S" RNBQKBNR" drop 8 0 do count 0 i piece loop drop
    8 0 do '`' acs 1 i piece loop
    S" RNBQKBNR" drop 8 0 do count 7 i piece loop drop
    8 0 do '`' acs 6 i piece loop
;

: board
    20 0 mv s" lqqqwqqqwqqqwqqqwqqqwqqqwqqqwqqqk" acs-type
    8 0 do i row i break loop
    36 0 mv s" mqqqvqqqvqqqvqqqvqqqvqqqvqqqvqqqj" acs-type
    \ pieces
    37 0 mv
;


: test
    initscr
    s" hello world" addstr

    ACS_BLOCK addch
    ACS_HLINE addch
    ACS_VLINE addch

    A_BOLD attron
    s" Bold" addstr
    A_BOLD attroff
    [char] ! A_BOLD + addch

    +I s" Italics" addstr -I

    +b +i s" bold+italics" addstr -b -i

    start-color
    0 0 0 0 init-color ( actually black )
    8 1 do  i i 0 init-pair  loop
    8 0 do 'A' i + i color-pair + addch loop

    3 0 mv acs-test
    box
    box2
    bigbox

    board

    refresh ;

: q endwin bye ;
