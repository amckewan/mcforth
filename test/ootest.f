( oo testing )

\ include lib/locals.f
include lib/oo.f
include test/tester.f
verbose on
decimal

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
{ rr print cr -> }


TESTING access to super ivars

point subclass point2
    m: get x @ y @ ;m
end-class

{ point2 var p2 -> }
{ p2 get -> 0 0 }
{ 88 99 p2 put -> }
{ p2 get -> 88 99 }


TESTING self super

point subclass point3
    m: size  self get * ;m
end-class

{ point3 var p3 -> }
{ 3 4 p3 put -> }
{ p3 size -> 12 }

point subclass inverted-point
    m: put  swap super put  ;m
end-class

{ inverted-point var inv -> }
{ 3 4 inv put -> }
{ inv get -> 4 3 }


TESTING object initialization

point subclass fixed-point
    m: init  100 x !  200 y !  ;m
end-class

{ fixed-point var fixed -> }
{ fixed get -> 100 200 }

\ init with args
\ this is risky until we figure out
\ a good way to do this for ivars
\ but it const almost nothing so we can keep it

point subclass initialized-point
    m: init ( x y -- )  y !  x !  ;m
end-class

{ 111 222 initialized-point var ip -> }
{ ip get -> 111 222 }
