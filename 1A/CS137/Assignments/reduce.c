#include <stdio.h>

int gcd(int a, int b) {
    int c;
    while ((c = a % b) != 0) {
        a = b;
        b = c;
    }
    return b;
}

int main(int argc, char *argv[]) {
    int a, b;
    scanf("%d%d", &a, &b);

    int sign = (a < 0) ^ (b < 0) ? -1 : 1;
    a = abs(a);
    b = abs(b);

    if (a == 0) {
        printf("0\n");
        return 0;
    } else if (b == 0) {
        printf("Divide by zero!\n");
        return 0;
    }

    int tgcd = gcd(a, b);

    int c = a / b;
    a -= c * b;

    a /= tgcd;
    b /= tgcd;

    if (c == 0)
        printf("%d/%d", sign * a, b);
    else if (b != 1)
        printf("%d %d/%d", c * sign, a, b);
    else
        printf("%d", c * sign);

    return 0;
}