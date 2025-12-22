// curses test
#include <curses.h>

int main() {
    initscr();
    addch(ACS_BLOCK);
    addch(ACS_HLINE);
    addch(ACS_VLINE);
    refresh();
    getch();
    endwin();

    for (int i = 'a'; i < 'z'; i++) {
        printf("%c (%d): %c (%d)\n", i, i, acs_map[i], acs_map[i]);
    }

    return 0;
}