#include <stdio.h>
#include <stdlib.h>

int max (int array[], int n) {
    if (n == 0) return 0;
    int i, max = -9999999;

    for (i = 0; i < n; i++)
        if (array[i] > max)
            max = array[i];

    return max;
}

int countValue (int array[], int n, int value) {
    int i, count = 0;

    for (i = 0; i < n; i++)
        if (array[i] == value)
            count++;

    return count;
}

void absolute (int array[], int n) {
    int i;

    for (i = 0; i < n; i++)
        if (array[i] < 0)
            array[i] *= -1;
}

int isSorted (int array[], int n) {
    if (n == 0) return 1;
    int i, max = -9999999;

    for (i = 0; i < n; i++) {
        if (array[i] < max)
            return 0;
        max = array[i];
    }

    return 1;
}

int isPermutation (int array[], int n) {
    if (n == 0) return 1;

    int i, *confirmed = calloc(sizeof(int), n);

    for (i = 0; i < n; i++) {
        if (!(array[i] > 0 && array[i] <= n)) {
            free(confirmed);
            return 0;
        }
        confirmed[array[i] - 1] = 1;
    }

    for (i = 0; i < n; i++)
        if (confirmed[i] == 0) {
            free(confirmed);
            return 0;
        }

    free(confirmed);
    return 1;
}

/*int main (void)
{
  int i;
  int a[0] = {};
  int b[5] = {-1, 0, 0, 0, 1};
  int c[5] = {-10, 9, -8 , 7, -6};

  printf("max(a, 5) = %d\n", max(a, 0));
  printf("countValue (a, 5, 1) = %d\n", countValue (a, 0, 1));
  printf("countValue (a, 5, 0) = %d\n", countValue (a, 0, 0));
  printf("isSorted (a, 5) = %d\n", isSorted (a, 0));
  printf("isPermutation (a, 5) = %d\n", isPermutation (a, 0));

  printf("max(b, 5) = %d\n", max(b, 5));
  printf("countValue (b, 5, 1) = %d\n", countValue (b, 5, 1));
  printf("countValue (b, 5, 0) = %d\n", countValue (b, 5, 0));
  printf("isSorted (b, 5) = %d\n", isSorted (b, 5));
  printf("isPermutation (b, 5) = %d\n", isPermutation (b, 5));

  absolute (c, 5);
  for (i = 0; i < 5; i++)
    printf ("c[%d] = %d\n", i, c[i]);
  return 0;
}*/
