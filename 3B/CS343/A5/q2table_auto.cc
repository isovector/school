#include "q2table.h"
using namespace std;

// A helper function to make our WAITUNTIL's before action simpler
void before_waiting(size_t left, size_t right, bool &waiting, Printer &printer) {
    // If we are waiting and weren't previously, inform the printer
    if (!waiting) {
        printer.print(left, Printer::Waiting, left, right);
        waiting = true;
    }
}

void Table::pickup(unsigned int id) {
    size_t left = id;
    size_t right = (id + 1) % numPhil;
    bool waiting = false;
    
    WAITUNTIL(forksAvailable[left] && forksAvailable[right], before_waiting(left, right, waiting, printer), )

    forksAvailable[left] = false;
    forksAvailable[right] = false;
    
    RETURN()
}

void Table::putdown(unsigned int id) {
    size_t left = id;
    size_t right = (id + 1) % numPhil;
    
    forksAvailable[left] = true;
    forksAvailable[right] = true;
    
    RETURN()
}
