#ifndef Q2ELF_H
#define Q2ELF_H

#include "q2workshop.h"
#include "q2printer.h"

_Task Elf {
    size_t id;
    Workshop &workshop;
    Printer &printer;
    size_t numConsultations;
    
    void print(Printer::States state) const;
    void main();
    
  public:
    enum { CONSULTING_GROUP_SIZE = 3 };
    Elf(size_t id, Workshop &wrk, Printer &prt, size_t numConsultations);
};

#endif
