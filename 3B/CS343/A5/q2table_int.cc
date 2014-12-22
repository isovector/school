#include "q2table.h"
using namespace std;

void Table::pickup(unsigned int id) {
    size_t left = id;
    size_t right = (id + 1) % numPhil;
    bool waiting = false;
    
    // Explicitly block until we can get both forks
    while (!forksAvailable[left] || !forksAvailable[right]) {
        if (!waiting) {
            // The first time we block, show a message to the printer
            printer.print(id, Printer::Waiting, left, right);
            waiting = true;
        }
        
        tableLock.wait();
    }
    
    forksAvailable[left] = false;
    forksAvailable[right] = false;
}

void Table::putdown(unsigned int id) {
    size_t left = id;
    size_t right = (id + 1) % numPhil;
    
    forksAvailable[left] = true;
    forksAvailable[right] = true;
    
    // inform anyone blocking on forks
    while (!tableLock.empty()) {
        tableLock.signal();
    }
}
