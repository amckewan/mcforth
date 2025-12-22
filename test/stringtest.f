( string class tests )

need string from lib/string.f
need testing from test/tester.f
verbose on
decimal

TESTING string init
{ string var s -> }
{ s length -> 0 }
{ s capacity -> 0 }
{ s data -> 0 }

TESTING string put
{ s" hello" s put -> }
{ s length -> 5 }
{ s data 0= -> false }
{ s capacity 5 < not -> true }
{ s get s" hello" compare -> 0 }

TESTING string grow
{ s" this is much longer" s put -> }
{ s length 5 > -> true }
{ s data 4 s" this" compare -> 0 }

TESTING string append
{ s" hello" s put -> }
{ s"  there" s append -> }
{ s get s" hello there" compare -> 0 }

TESTING string equal
{ s" hello" s put -> }
{ s" hello" s equal -> true }
{ s" goodbye" s equal -> false }

TESTING string starts-with
{ s" hello" s put -> }
{ s" " s starts-with -> true }
{ s" hel" s starts-with -> true }
{ s" no" s starts-with -> false }
{ s" hello there" s starts-with -> false }

TESTING string ends-with
{ s" hello, world" s put -> }
{ s" " s ends-with -> true }
{ s" world" s ends-with -> true }
{ s" xorld" s ends-with -> false }
{ s" worlds" s ends-with -> false }
{ s" hello" s ends-with -> false }
