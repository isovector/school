#include <stdio.h>
#include <stdlib.h>

int *histogram (const int *a, int n, int *m) {
    int *output, i = 0;
    
    for (i = 0; i < n; i++)
        if (a[i] >= *m)
            *m = a[i];
    if (*m == 0)
        return 0;
    ++*m;
    output = (int*)calloc(*m, sizeof(int));
    for (i = 0; i < n; i++)
        if (a[i] >= 0)
            output[a[i]]++;
    return output;
}

/*int main (void) {
  int a[] = {1, 2, 3, 3, 3, 2, 1, 4, 5, 6, 0, -100};
  int *h, m, i;

  h = histogram (a, sizeof(a)/sizeof(a[0]), &m);

  if (h)
    for (i = 0; i < m; i++)
      printf ("%d\n", h[i]);

  free (h);

  return 0;
}*/
