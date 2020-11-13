TESTING VALUE
{  111 VALUE v1 -> }
{ -999 VALUE v2 -> }
{ v1 ->  111 }
{ v2 -> -999 }
{ 222 TO v1 -> }
{ v1 -> 222 }
{ : vd1 v1 ; -> }
{ vd1 -> 222 }

{ : vd2 TO v2 ; -> }
{ v2 -> -999 }
{ -333 vd2 -> }
{ v2 -> -333 }
{ v1 ->  222 }

TESTING DEFER
{ DEFER defer2 ->   }
{ ' * ' defer2 DEFER! -> }
{   2 3 defer2 -> 6 }
{ ' + IS defer2 ->   }
{    1 2 defer2 -> 3 }
{ : defer1 ['] - is defer2 ; -> }
{ defer1 -> }
{    1 2 defer2 -> -1 }
