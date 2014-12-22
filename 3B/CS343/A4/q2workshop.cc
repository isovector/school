#include "q2workshop.h"
#include <iostream>
using namespace std;

static const size_t MAX_NUM_REINDEER = 5;
static const size_t CONSULTING_GROUP_SIZE = 3;

Workshop::Workshop(Printer &prt, size_t N, size_t E, size_t D) 
    : printer(prt), N(N), E(E), D(D), n(0), elvesRemaining(E)
{
}

Workshop::Status Workshop::sleep() {
    do {
        if (reindeerWaiting.size() == MAX_NUM_REINDEER) {
            for (size_t i = 0; i < reindeerWaiting.size(); ++i) {
                theImpatienceOfBeasts.signal();
            }

            return Delivery;
        } else if (elvesWaiting.size() >= CONSULTING_GROUP_SIZE) {
            for (size_t i = 0; i < CONSULTING_GROUP_SIZE; ++i) {
                theBusyElves.signal();
            }
            
            return Consulting;
        }
        
        if (elvesRemaining == 0 && D == 0) {
            return Done;
        }
        
        theSlumberOfDarkThings.wait();
    } while(true);
}

void Workshop::deliver(size_t id) {
    static size_t blocked = 0;
    
    reindeerWaiting.push_back(id);
    if (reindeerWaiting.size() == MAX_NUM_REINDEER) {
        theSlumberOfDarkThings.signal();
    }
    
    printer.print(id, Printer::Blocked, ++blocked);
    theImpatienceOfBeasts.wait();
    printer.print(id, Printer::Unblocked, 5 - --blocked);
}

bool Workshop::consult(size_t id) {
    static size_t blocked = 0;
    
    if (elvesRemaining < CONSULTING_GROUP_SIZE) {
        return false;
    }
    
    elvesWaiting.push_back(id);
    if (elvesWaiting.size() == CONSULTING_GROUP_SIZE) {
        theSlumberOfDarkThings.signal();
    }
    
    printer.print(id, Printer::Blocked, ++blocked);
    theBusyElves.wait();
    printer.print(id, Printer::Unblocked, E - --blocked);
    
    return true;
}

void Workshop::doneConsulting(size_t id) {
    elvesWaiting.pop_front();
}

void Workshop::doneDelivering(size_t id) {
    reindeerWaiting.pop_front();
    if (reindeerWaiting.size() == 0) {
        --D;
    }
    
    if (D == 0) {
        theSlumberOfDarkThings.signal();
    }
}

void Workshop::termination(size_t id) {
    --elvesRemaining;
}
