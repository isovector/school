#include <stdlib.h>

struct poly {
    int degree;
    double *coef;
};

struct poly *polyCreate();
struct poly *polyDelete(struct poly *p);
struct poly *polySetCoefficient (struct poly *p, int i, double value);
double polyGetCoefficient (struct poly *p, int i);
int polyDegree (struct poly *p);
void polyPrint (struct poly *p);
struct poly *polyCopy (struct poly *p);
struct poly *polyAdd (struct poly *p0, struct poly *p1);
struct poly *polyMultiply (struct poly *p0, struct poly *p1);
struct poly *polyPrime (struct poly *p);
double polyEval (struct poly *p, double x);
