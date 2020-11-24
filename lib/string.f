( string )

need object from lib/object.f

: malloc ( m -- a )  allocate abort" malloc?" ;

class string

cell bytes length
cell bytes data
cell bytes capacity

method data ( -- data )  cell+ @ ;
method length ( -- n )  @ ;
method capacity ( -- n )  2 cells + @ ;

method at ( index -- char )  cell+ @ + c@ ;

m: reserve ( n -- )
    dup capacity @ > if
        dup malloc
        data @ over length @ move
        data @ free drop
        data !
        dup capacity !
    then drop ;m

method clear  0 swap ! ;

method get ( -- addr len )  2@ ;

m: put  ( addr len -- )
    dup self reserve
    dup length !
    data @ swap move ;m

m: append  ( addr len -- )
    dup length @ + self reserve
    length 2@ + swap  dup length +!  move ;m

method equal ( addr len -- f )
    2@ compare 0= ;

end-class

string var s
