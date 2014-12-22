#include <stdio.h>
#include <math.h>

void polyPrint (double a[], int n) {
    int i;

    if (n == 0) {
        printf("0\n");
        return;
    }

    for (i = n - 1; i >= 0; i--)
        if (a[i] != 0 || i == 0) {
            if (i == 0)
                printf("%g", a[i]);
            else if (i == 1)
                if (a[i] == 1)
                    printf("x");
                else if (a[i] == -1)
                    printf("-x");
                else
                    printf("%gx", a[i]);
            else
                if (a[i] == 1)
                    printf("x^%d", i);  
                else if (a[i] == -1)
                    printf("-x^%d", i);  
                else
                    printf("%gx^%d", a[i], i);
            break;
        }
    for (i--; i >= 0; i--)
        if (a[i] != 0) {
            if (i == 0)
                printf(" %c %g", a[i] < 0 ? '-' : '+', fabs(a[i]));
            else if (i == 1)
                if (fabs(a[i]) != 1)
                    printf(" %c %gx", a[i] < 0 ? '-' : '+', fabs(a[i]));
                else
                    printf(" %c x", a[i] < 0 ? '-' : '+');
            else
                if (fabs(a[i]) != 1)
                    printf(" %c %gx^%d", a[i] < 0 ? '-' : '+', fabs(a[i]), i); 
                else
                    printf(" %c x^%d", a[i] < 0 ? '-' : '+', i); 
        }

    printf("\n");
}

/*int main (void)
{
  double a[] = {2.0, 3.0, 4.0};
  double b[] = {0.0, 3.0, 0.0};
  double c[] = {2.0, -2.0, 9.0, -1.0, 8.0, -7.0};
  double d[] = {2.0, 0.0, 0.0, 0.0, 0.0, 1.0};
  double e[] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  double f[] = {2.0/3.0, 1.0/7.0, 9.0/13.0};

  polyPrint (a, 1);
  polyPrint (b, sizeof(b)/sizeof(b[0]));
  polyPrint (c, sizeof(c)/sizeof(c[0]));
  polyPrint (d, sizeof(d)/sizeof(d[0]));
  polyPrint (e, sizeof(e)/sizeof(e[0]));
  polyPrint (f, sizeof(f)/sizeof(f[0]));

  return 0;
}*/
