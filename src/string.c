/* strings */

#include "fo.h"

/*
    COMPARE  ( c-addr1 u1 c-addr2 u2 -- n )
    Compare the string specified by c-addr1 u1 to the string specified by
    c-addr2 u2. The strings are compared, beginning at the given
    addresses, character by character, up to the length of the shorter
    string or until a difference is found. If the two strings are
    identical, n is zero. If the two strings are identical up to the
    length of the shorter string, n is minus-one (-1) if u1 is less than
    u2 and one (1) otherwise. If the two strings are not identical up to
    the length of the shorter string, n is minus-one (-1) if the first
    non-matching character in the string specified by c-addr1 u1 has a
    lesser numeric value than the corresponding character in the string
    specified by c-addr2 u2 and one (1) otherwise.
*/

int compare(const char *a1, int n1, const char *a2, int n2) {
    int n = n1 < n2 ? n1 : n2;
    int d = memcmp(a1, a2, n);
    if (d < 0) d = -1;
    else if (d > 0) d = 1;
    else if (n1 != n2) d = n1 < n2 ? -1 : 1;
    return d;
}

/*
    SEARCH  ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
    Search the string specified by c-addr1 u1 for the string specified by
    c-addr2 u2. If flag is true, a match was found at c-addr3 with u3
    characters remaining. If flag is false there was no match and c-addr3
    is c-addr1 and u3 is u1.
*/

int search(cell *S, cell top) {
    const char *str = abs(S[2]);
    unsigned len = S[1];
    const char * const pattern = abs(S[0]);
    const unsigned pattern_len = top;

    if (pattern_len == 0) {
        top = TRUE; // empty pattern always matches
    } else {
        top = FALSE;
        // look in str for the first character of pattern, then compare
        const char c = *pattern;
        const char *first;
        while ((first = memchr(str, c, len))) {
            len -= first - str;
            str = first;
            if (len < pattern_len) break; // not enough room
            if (!memcmp(str, pattern, pattern_len)) {
                S[2] = rel(str);
                S[1] = len;
                top = TRUE;
                break;
            }
            ++str;
            --len;
        }
    }
    return top;
}
