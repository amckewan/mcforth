TESTING STRINGS

[DEFINED] /STRING [IF]
{ s" abcdef" nip -> 6 }
{ s" abcdef" 2 /string nip -> 4 }
{ s" abcdef" -2 /string nip -> 8 }
[THEN]

0 [IF]
COMPARE  ( c-addr1 u1 c-addr2 u2 -- n )
Compare the string specified by c-addr1 u1 to the string specified by c-addr2 u2. The strings are compared, beginning at the given addresses, character by character, up to the length of the shorter string or until a difference is found. If the two strings are identical, n is zero. If the two strings are identical up to the length of the shorter string, n is minus-one (-1) if u1 is less than u2 and one (1) otherwise. If the two strings are not identical up to the length of the shorter string, n is minus-one (-1) if the first non-matching character in the string specified by c-addr1 u1 has a lesser numeric value than the corresponding character in the string specified by c-addr2 u2 and one (1) otherwise.
[THEN]

{ s" " s" " compare -> 0 }
{ s" abc" s" abc" compare -> 0 }
{ s" abc" s" abcdef" compare -> -1 }
{ s" abcdef" s" abc" compare -> 1 }
{ s" abcdef" s" abzdef" compare -> -1 }
{ s" abzdef" s" abcdef" compare -> 1 }
