#ifndef Q2_PHILOSOPHER_H
#define Q2_PHILOSOPHER_H

#include "q2printer.h"
#include "q2table.h"

#include <vector>

_Task Philosopher {
    size_t id;
    size_t noodles;
    Table &table;
    Printer &printer;

    void print();
    void main();

  public:
    typedef Printer::States States;
    Philosopher( unsigned int id, unsigned int noodles, Table &table, Printer &prt );
};

#endif
