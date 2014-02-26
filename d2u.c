#include <stdio.h>

int main(void) {
    char c;
    while((c = getchar()) != EOF) {
        if(c == 13) continue;
        putchar(c);
    }
    return 0;
}
