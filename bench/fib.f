: fib ( n1 -- n2 ) recursive
    dup 2 < if
	   drop 1
    else
	   dup
	   1- fib
	   swap 2 - fib
	   +
    then ;

: main 38 fib . cr ;
