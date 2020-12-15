( standard structures )

: begin-structure ( <name> -- addr 0 )
    create here 0 , 0  does> @ ;

: end-structure ( addr n -- )
    swap ! ;

: +field ( n <name> -- )
    create over , +  does> @ + ;

: field: ( <name> -- )
    aligned cell +field ;

\S testing

begin-structure point
    field: x
    field: y
end-structure

create p point allot

begin-structure rect
    point +field ul
    point +field lr
end-structure

create r rect allot

\S
cfield:
ffield:
sffield:
dffield:

bfield:
wfield:
lfield:
xfield:
