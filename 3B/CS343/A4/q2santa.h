#ifndef Q2SANTA_H
#define Q2SANTA_H

#include "q2workshop.h"
#include "q2printer.h"

_Task Santa {
    Workshop &workshop;
    Printer &printer;
    
    void print(Printer::States state) const;
    void main();
    
  public:
    Santa(Workshop &wrk, Printer &prt);
};

#endif
