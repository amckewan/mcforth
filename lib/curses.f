( curses )

0 value libcurses

: ?dlerror ( f -- )
    if  dlerror pad place  pad msg !  -2 throw  then ;

: open-curses  ( -- f )
    s" libncurses.so.6" dlopen  dup 0= ?dlerror  to libcurses ;

: close-curses ( -- )
    libcurses ?dup if  dlclose  0 to libcurses  then ;

: getsym ( addr len -- sym )
    libcurses dlsym dup 0= ?dlerror ;

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
