#ifndef Q2REINDEER_H
#define Q2REINDEER_H

#include "q2workshop.h"
#include "q2printer.h"

_Task Reindeer {
    size_t id;
    Workshop &workshop;
    Printer &printer;
    size_t numDeliveries;
    
    void print(Printer::States state) const;
    void main();
    
  public:
    enum { MAX_NUM_REINDEER = 5 };
    Reindeer(size_t id, Workshop &wrk, Printer &prt, size_t numDeliveries);
};

#endif
