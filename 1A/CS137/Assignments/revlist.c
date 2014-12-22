#include <stdio.h>
#include <stdlib.h>

void revlist() {
    int temp = 0;
    scanf("%d", &temp);
    if (temp != 0)
        revlist();
    printf("%d\n", temp);
    return;
}

int main(int argc, char *argv[]) {
    /*int *list = calloc(1024, sizeof(int)), *ptr = &list[0], temp = 0;

    while (scanf("%d", &temp), temp != 0) {
        *ptr = temp;
        ptr++;
    }
    *ptr = 0;

    while (ptr != list)
        printf("%d\n", *ptr--);
    printf("%d", *ptr);
    free(list);*/

    revlist();
    return 0;
}