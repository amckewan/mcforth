( test path )

need dirname from src/path.f
need testing from test/tester.f

TESTING dirname

{ s" " dirname s" ./" compare -> 0 }
{ s" ." dirname s" ./" compare -> 0 }
{ s" .." dirname s" ./" compare -> 0 }
{ s" /" dirname s" /" compare -> 0 }
{ s" /dir" dirname s" /" compare -> 0 }
{ s" /dir/" dirname s" /dir/" compare -> 0 }
{ s" /dir/file" dirname s" /dir/" compare -> 0 }
{ s" dir/file" dirname s" dir/" compare -> 0 }
{ s" file" dirname s" ./" compare -> 0 }

TESTING basename

{ s" " basename s" " compare -> 0 }
{ s" ." basename s" ." compare -> 0 }
{ s" .." basename s" .." compare -> 0 }
{ s" /" basename s" " compare -> 0 }
{ s" /dir" basename s" dir" compare -> 0 }
{ s" /dir/" basename s" " compare -> 0 }
{ s" /dir/file" basename s" file" compare -> 0 }
{ s" dir/file" basename s" file" compare -> 0 }
{ s" file" basename s" file" compare -> 0 }
