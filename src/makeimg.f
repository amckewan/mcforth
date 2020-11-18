( Create relocatable image )

: bin1 s" kernel0.bin" ;
: bin2 s" kernel.bin"  ;


$2000 constant maxsize
4 constant cell
: cell@ ul@ ;

create image1 0 , 0 ,
create image2 0 , 0 ,

: ?ERR  ABORT" file I/O error" ;

: OPEN  ( -- fid ) R/O OPEN-FILE ?ERR ;
: CLOSE ( fid -- ) CLOSE-FILE ?ERR ;

: READ  ( fname len -- data len )
    2dup cr ." Reading " type space
    open >r
    maxsize allocate ?err
    dup maxsize r@ read-file ?err
    r> close
    dup . ." bytes ok " ;

bin1 read image1 2!
bin2 read image2 2!

variable reloc
: compare
    image1 @ image2 @ - abort" sizes differ"
    image2 2@ drop image1 2@ 0 do ( a1 a1 )
        over cell@
        over cell@ -
        ?dup if cr ." offset " i . ." mod " i 3 and . ." diff " .  1 reloc +!  then

        cell + swap cell + swap
    cell +loop ;

hex compare
decimal cr image1 @ cell / . ." cells " reloc ? ." relocations" cr
bye
