#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int isPrime(int number) {
    int i;
    if (number <= 1) return 0;
    if (number == 2) return 1;
    if (number % 2 == 0) return 0;
    
    for (i = 3; i < number / 2; i += 2)
        if (number % i == 0)
            return 0;
    return 1;
}

int isTwin(int number) {
    return isPrime(number) && (isPrime(number - 2) || isPrime(number + 2));
}