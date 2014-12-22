#include <stdio.h>

int max(int a, int b) {
    return a > b ? a : b;
}

int min(int a, int b) {
    return a < b ? a : b;
}

int gcd(int a, int b) {
    int c;
    while ((c = a % b) != 0) {
        a = b;
        b = c;
    }
    return b;
}

int main(int argc, char *argv[]) {
    int a,b;
    scanf("%d%d", &a, &b);
    printf("%d", gcd(a, b));
    return 0;
}