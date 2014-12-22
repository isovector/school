#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int magic(int n, int power, int *outhighest) {
    int *storage, i = 0, j = 0, highest = 0, carry, sum = 0;
    storage = (int*)calloc(1000, sizeof(int));
    storage[0] = n;

    for (; i < power - 1; i++) {
        carry = 0;
        for (j = 0; j <= highest; j++) {
            storage[j] = storage[j] * n + carry;
            carry = storage[j] / 10000;
            storage[j] %= 10000;
            if (carry != 0 && j == highest)
                highest++;
        }
    }
    *outhighest = highest;
    for (i = 0; i <= highest; i++)
        while (storage[i] != 0) {
            sum += storage[i] % 10;
            storage[i] /= 10;
        }
    return sum;
}

int main(int argc, char* argv[]) {
    int a;
    printf("Magic of 2^1000 is %d\n", magic(2, 1000, &a));
    printf("Highest was %d\n", a);
    return 0;
}