( string words )

: S=  COMPARE 0= ;

: /STRING  ( a u n -- a' u' )  ROT OVER +  ROT ROT - ;

: -TRAILING  ( a n -- a n' )
    BEGIN  DUP WHILE  2DUP + 1- C@ BL = WHILE  1-  REPEAT THEN ;

: BLANK  BL FILL ;
