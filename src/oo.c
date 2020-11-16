/* misc support functions */

#include "fo.h"

#define THREADS 8 // # of method threads in the class

#define MFA(class)  class + CELL

cell find_method(cell methods, cell selector) {
    cell link = methods + selector & CELLS(THREADS - 1);
    while ((link = AT(link))) {
        if (AT(link + CELL) == selector) {
            return link + 2 * CELL;
        }
    }
    return 0;
}
