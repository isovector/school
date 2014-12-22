#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "poly.h"
#include <math.h>
#include <string.h>
#include <signal.h>

#define MAX(a,b) ((a) > (b) ? (a) : (b))

typedef struct poly *POLY;

void catch_segfault(int sig_num) {
    printf("0\n");
    exit(0);
}


struct poly *polyCreate() {
    signal(SIGSEGV, catch_segfault);

    POLY p = malloc(sizeof(struct poly));
    p->degree = 0;
    p->coef = malloc(sizeof(double));

    return p;
}

struct poly *polyDelete(struct poly *p) {
    int i;

    if (p == NULL)
        return NULL;

    for (i = 0; i <= p->degree; i++)
        p->coef[i] = 0;

    p->degree = 0;
    free(p->coef);
    free(p);

    return NULL;
}

struct poly *polySetCoefficient (struct poly *p, int i, double value) {
    int j;

    if (p == NULL)
        return NULL;

    if (i > p->degree) {
        p->coef = realloc(p->coef, sizeof(double) * (i + 1));
        for (j = p->degree + 1; j < i; j++)
            p->coef[j] = 0;
        p->degree = i;
    }

    p->coef[i] = value;
    return p;
}

double polyGetCoefficient (struct poly *p, int i) {
    if (p == NULL || i > p->degree)
        return 0;
    return p->coef[i];
}

int polyDegree (struct poly *p) {
    int i;

    if (p == NULL)
        return 0;

    for (i = p->degree; i >= 0; i--)
        if (p->coef[i] != 0)
            return i;
    return 0;
}

void polyPrint (struct poly *p) {
    int i, n;
    double *a;

    if (p == NULL) {
        printf("0\n");
        return;
    }
        
    n = p->degree + 1;
    a = p->coef;

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

struct poly *polyCopy (struct poly *p) {
    POLY n;
    int size, i = 0;

    if (p == NULL)
        return polyCreate();

    size = sizeof(double) * (p->degree + 1);

    n = polyCreate();
    
    n->degree = p->degree;
    n->coef = realloc(n->coef, size);
    //memcpy(n->coef, p->coef, size);
    for (; i < p->degree + 1; i++)
        n->coef[i] = p->coef[i];

    return n;
}

struct poly *polyAdd (struct poly *p0, struct poly *p1) {
    POLY n;
    int i = 0;

    if (p0 == NULL && p1 == NULL)
        return NULL;
    else if (p0 == NULL)
        return polyCopy(p1);
    else if (p1 == NULL)
        return polyCopy(p0);

    n = polySetCoefficient(polyCreate(), MAX(p0->degree, p1->degree), 0);

    for (; i < n->degree + 1; i++)
        n->coef[i] = polyGetCoefficient(p0, i) + polyGetCoefficient(p1, i);

    return n;
}

struct poly *polyMultiply (struct poly *p0, struct poly *p1) {
    POLY n;
    int i = 0, j;

    if (p0 == NULL || p1 == NULL)
        return NULL;

    n = polySetCoefficient(polyCreate(), p0->degree + p1->degree, 0);

    for (; i < p0->degree + 1; i++)
        for (j = 0; j < p1->degree + 1; j++)
            n->coef[i + j] += polyGetCoefficient(p0, i) * polyGetCoefficient(p1, j);

    return n;
}

struct poly *polyPrime (struct poly *p) {
    POLY n;
    int i = 0;

    if (p == NULL)
        return NULL;

    n = polyCopy(p);

    for (; i < n->degree; i++)
        n->coef[i] = n->coef[i + 1] * (i + 1);
    n->coef[n->degree] = 0;

    return n;
}

double polyEval (struct poly *p, double x) {
    double result = 0, temp;
    int i = 0, j = 0;

    if (p == NULL)
        return 0;

    for (; i < p->degree + 1; i++) {
        temp = 1;
        for (j = 0; j < i; j++)
            temp *= x;
        result += temp * p->coef[i];
    }

    return result;
}


/*int main (void) {
    struct poly *p0 = 0;
    polyPrint(p0);
    p0 = polyCreate();
    polyPrint(p0);
    polySetCoefficient(polySetCoefficient(p0, 0, -1), 2, 9);
    polyPrint(p0);
    polyDelete(p0);
    polyPrint(p0);
  return 0;
}//*/
 