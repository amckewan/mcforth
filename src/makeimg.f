( Create relocatable image )

\ warnings off

: bin1 s" kernel0.bin" ;
: bin2 s" kernel1.bin"  ;


$2000 constant maxsize
4 constant cell
: cell@ ul@ ;

create image1 0 , 0 ,
create image2 0 , 0 ,

: ?ERR  ABORT" file I/O error" ;

: OPEN  ( -- fid ) R/O OPEN-FILE ?ERR ;
: WRITE ( a n fid -- )  WRITE-FILE ?ERR ;
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

: check-sizes
    image1 @ image2 @ - abort" sizes differ" ;
check-sizes

image1 2@ constant size constant addr1
image2 2@ drop constant addr2

\ start simple, one byte per image byte, 1=relocate
size allocate ?err constant reloc
reloc size erase

s" reloc.bin" w/o create-file ?err constant out

variable #reloc
: compare
    addr2 addr1  size 0 do
        over cell@
        over cell@ -
        ?dup if
            cr ." offset " i 4 .r ."  mod " i 3 and . ." diff " .
            1 #reloc +!
            1 i reloc + c!
        then

        cell + swap cell + swap
    cell +loop 2drop ;

hex compare
decimal cr size cell / . ." cells " #reloc ? ." relocations" cr

reloc size out write
out close
bye
