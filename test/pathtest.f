( test path )

need dirname from src/path.f
need testing from test/tester.f

TESTING dirname

T{ s" " dirname s" ./" compare -> 0 }T
T{ s" ." dirname s" ./" compare -> 0 }T
T{ s" .." dirname s" ./" compare -> 0 }T
T{ s" /" dirname s" /" compare -> 0 }T
T{ s" /dir" dirname s" /" compare -> 0 }T
T{ s" /dir/" dirname s" /dir/" compare -> 0 }T
T{ s" /dir/file" dirname s" /dir/" compare -> 0 }T
T{ s" dir/file" dirname s" dir/" compare -> 0 }T
T{ s" file" dirname s" ./" compare -> 0 }T

TESTING basename

T{ s" " basename s" " compare -> 0 }T
T{ s" ." basename s" ." compare -> 0 }T
T{ s" .." basename s" .." compare -> 0 }T
T{ s" /" basename s" " compare -> 0 }T
T{ s" /dir" basename s" dir" compare -> 0 }T
T{ s" /dir/" basename s" " compare -> 0 }T
T{ s" /dir/file" basename s" file" compare -> 0 }T
T{ s" dir/file" basename s" file" compare -> 0 }T
T{ s" file" basename s" file" compare -> 0 }T
