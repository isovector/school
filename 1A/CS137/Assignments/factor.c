#include <stdio.h>

int isPrime(int number) {
    int i;
    if (number <= 1) return 0;
    if (number == 2) return 1;
    if (number % 2 == 0) return 0;
    
    for (i = 3; i * i < number + 1; i += 2)
        if (number % i == 0)
            return 0;
    return 1;
}

void factor (int number) {
    int i;
    printf("%d = ", number);

    while (!isPrime(number)) {
        for (i = 2; i * i < number + 1; i++) {
            if (number % i == 0) {
                printf("%d*", i);
                number /= i;
                break;
            }
        }
    }
    printf("%d\n", number);
}

int main (void) {
  factor (81);
  factor (67);
  factor (78);
//  factor (2769);
  return 0;
}
