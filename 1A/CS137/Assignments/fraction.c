#include <stdio.h>
#include "fraction.h"

int gcd(int a, int b) {
    int c;
    if (b == 0)
        return 1;
    while ((c = a % b) != 0) {
        a = b;
        b = c;
    }
    return b;
}

struct fraction fractionCreate(int numerator, double denominator) {
    int denom = (int)denominator, tgcd;
    if (denom == 0)
        return (struct fraction) {0, 0};
    tgcd = gcd(numerator, denom);
    return (struct fraction) {numerator / tgcd, denom / tgcd};
}

struct fraction fractionAdd(struct fraction a, struct fraction b) {
    return fractionCreate(a.top * b.bottom + b.top * a.bottom, a.bottom * b.bottom);
}

struct fraction fractionSubtract(struct fraction a, struct fraction b) {
    return fractionCreate(a.top * b.bottom - b.top * a.bottom, a.bottom * b.bottom);
}

struct fraction fractionMultiply(struct fraction a, struct fraction b) {
    return fractionCreate(a.top * b.top, a.bottom * b.bottom);
}

struct fraction fractionDivide(struct fraction a, struct fraction b) {
    return fractionCreate(a.top * b.bottom, a.bottom * b.top);
}

void fractionPrint(struct fraction f) {
    int a = f.top, b = f.bottom;

    int sign = (a < 0) ^ (b < 0) ? -1 : 1;
    a = abs(a);
    b = abs(b);

    if (b == 0) {
        printf("Divide by zero!\n");
        return;
    } else if (a == 0) {
        printf("0\n");
        return;
    }

    int tgcd = gcd(a, b);

    int c = a / b;
    a -= c * b;

    a /= tgcd;
    b /= tgcd;

    if (c == 0)
        printf("%d/%d\n", sign * a, b);
    else if (b != 1)
        printf("%d %d/%d\n", c * sign, a, b);
    else
        printf("%d\n", c * sign); 
}

int main (void) {
  struct fraction sum = fractionCreate(0, 1);
    int i;

    for (i = 1;  i < 100; i++) {
        sum = fractionAdd(sum, fractionCreate(1, i));
    }

    fractionPrint(sum);
  return 0;
}
