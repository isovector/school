#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    int number, rank, output = 0, sign = 1;
    scanf("%d", &number);

    if (number < 0)
        sign = -1;

    number *= sign;
    
    while (number != 0) {
        output *= 10;
        output += number - (number / 10 * 10);
        number /= 10;
        rank++;
    }

    int output2 = output, i;

    while (output2 != 0) {
        output2 /= 10;
        rank--;
    }

    for (i = 0; i < rank; i++)
        printf("0");

    printf("%d\n", output * sign);

    return 0;
}