#include <stdio.h>
#include <math.h>

void bowtie(int n) {
    int y = 0, x = 0, p = -n + 1;

    for (y = 0; y < n; y++) {
        for (x = 0; x < 2 * n; x++)
            printf("%c", (x <= y || 2*n-1-y <= x)?'*':' '); 
        p++;
        printf("\n");
    }
    for (y = n - 2; y >= 0; y--) {
        for (x = 0; x < 2 * n; x++)
            printf("%c", (x <= y || 2*n-1-y <= x)?'*':' '); 
        p++;
        printf("\n");
    }
}

/*
int main (void) {
  bowtie (1);
  printf ("\n");
  bowtie (4);
  return 0;
}*/
