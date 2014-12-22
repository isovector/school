#include <stdio.h>
#include <math.h>

int iPart(double m) {
    return (int)m;
}

int fPart(double m) {
    return (m - (int)m) * 100;
}


void printAverageAmount(double money[], int n) {
    long sum = 0, dollars = 0;
    int i = 0;

    for (i = 0; i < n; i++)  {
        //printf(">%d\n", (long)(money[i] * 100.0));
        sum += (long)round(money[i] * 100.0);
    }
    sum -= sum % n;

    //printf("%d\n", sum);

    sum /= n;

    dollars = sum / 100;
    sum %= 100;

    printf("Everyone gets %d dollar(s) and %d cent(s).\n", dollars, sum);
}

/*int main (void) {
  double a[5] = {100.00, 250.00, 320.00, 120.00, 1500.00};
  double b[3] = {8.00, 1.00, 1.00};
  double c[3] = {8.00, 1.50, 1.50};

  
  printAverageAmount(a, 5);
  printAverageAmount(b, 3);
  printAverageAmount(c, 3);
  
  return 0;
}*/
