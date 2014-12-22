#ifndef FRACTION_H
#define FRACTION_H

struct fraction {
    int top;
    int bottom;
};

struct fraction fractionCreate (int numerator, double denominator);
struct fraction fractionAdd (struct fraction a, struct fraction b);
struct fraction fractionSubtract (struct fraction a, struct fraction b);
struct fraction fractionMultiply (struct fraction a, struct fraction b);
struct fraction fractionDivide (struct fraction a, struct fraction b);
void fractionPrint (struct fraction f); 

#endif
