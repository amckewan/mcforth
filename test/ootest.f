( oo testing)

\ include lib/locals.f
include lib/oo.f
include test/tester.f
verbose on

TESTING object
Object var o

{ o init -> }
{ o print cr -> }

TESTING simple class

object subclass point
    cell bytes y
    cell bytes x

    method get  2@ ;
    method put  2! ;

    m: print  x @ 0 .r '@' emit y ? ;m
end-class

{ point var p }
{ p get -> 0 0 }
{ 3 4 p put -> }
{ p get -> 3 4 }
{ p print cr -> }

TESTING class ivars

class rect
    point var p1
    point var p2
    m: get  p1 get  p2 get ;m
    m: put  p2 put  p1 put ;m
    m: print  p1 print  p2 print  ;m
    m: setupr ( pt -- )  get p1 put ;m
end-class

{ rect var r }
{ r get -> 0 0 0 0 }
{ 1 2 3 4 r put -> }
{ r get -> 1 2 3 4 }
{ r print cr -> }

TESTING subclass

rect subclass crect
    cell bytes color
    m: get super get color @ ;m
    m: print  super print  ." color " color ? ;m
    m: setcolor  color ! ;m
end-class

{ crect var rr }
{ rr get -> 0 0 0 0 0 }
{ 11 22 33 44 rr put -> }
{ 55 rr setcolor -> }
{ rr get -> 11 22 33 44 55 }
{ rr print -> }
