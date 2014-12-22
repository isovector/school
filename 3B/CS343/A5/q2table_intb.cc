#include "q2table.h"
using namespace std;

void Table::pickup(unsigned int id) {
    size_t left = id;
    size_t right = (id + 1) % numPhil;
    bool isWaiting = false;
    
    // Exactly the same as q2table_int.cc's, except for the wait() call
    while (!forksAvailable[left] || !forksAvailable[right]) {
        if (!isWaiting) {
            printer.print(id, Printer::Waiting, left, right);
            isWaiting = true;
        }
        
        wait();
    }
    
    forksAvailable[left] = false;
    forksAvailable[right] = false;
}

void Table::putdown(unsigned int id) {
    size_t left = id;
    size_t right = (id + 1) % numPhil;
    
    forksAvailable[left] = true;
    forksAvailable[right] = true;
    
    for (size_t i = 0; i < numPhil; ++i) {
        waiting.signal();
    }
}

// Given by assignment
void Table::wait() {
    waiting.wait();
    while ( rand() % 5 == 0 ) {
        _Accept( pickup, putdown ) {
        } _Else {
        }
    }
}
