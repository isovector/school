#include "q2table.h"
using namespace std;

// We need to create a forklock for each philosopher
// (see q2table.cc)
#define TABLE_PHIL_LOOP(id) forkLocks.push_back(new uCondition());

Table::~Table() {
    for (size_t i = 0; i < forkLocks.size(); ++i) {
        // prevent memory leaks
        delete forkLocks[i];
    }
}

void Table::main() {
    while (true) {
        _Accept(pickup) {
            // see if anyone wants forks
            relinquishForkMonopoly();
        } 
        or
        _Accept(putdown) {
            // return forks
            size_t left = requestedId;
            size_t right = (requestedId + 1) % numPhil;
            
            forksAvailable[left] = true;
            forksAvailable[right] = true;
            
            // see if anyone wants forks
            relinquishForkMonopoly();
        }
        or
        _Accept(~Table) {
            // get out of here, ya dork!
            return;
        }
    }
}

// Checks all philosophers queued for forks, and attempts to give them out if possible
void Table::relinquishForkMonopoly() {
    // iterate through all waiting philosophers
    for (deque<int>::iterator it = hungryPhils.begin(); it < hungryPhils.end(); ++it) {
        // calculate id (see setWaiting for what's going on here)
        size_t id = static_cast<size_t>(abs(*it)) - 1;
        size_t left = id;
        size_t right = (left + 1) % numPhil;
        
        if (forksAvailable[left] && forksAvailable[right]) {
            forksAvailable[left] = false;
            forksAvailable[right] = false;
            
            // tell the philosopher to get back to business
            forkLocks[id]->signalBlock();
            
            // remove the philosopher from the waiting queue
            hungryPhils.erase(it++);
        } else if (*it > 0) {
            // the philosohper can't get his forks, so inform the printer
            printer.print(id, Printer::Waiting, left, right);
            setWaiting(id);
        }
    }
}

// Tells the table that a philosopher is waiting.
// The first time this is called, the philosopher will be added to the queue
// if called again, and the philosopher is still in the queue, mark the 
// philosopher as "officially waiting" (ie, don't tell the printer again)
void Table::setWaiting(size_t id) {
    // The logic here is kind of gross. We store queued philosophers with their positive
    // id, and waiting philosophers with negative ids. 
    // To accomplish this, all ids must be +1, since there is no -0
    
    // When iterating through this queue, id = static_cast<size_t>(abs(*it)) - 1
    id += 1;
    
    for (deque<int>::iterator it = hungryPhils.begin(); it != hungryPhils.end(); ++it) {
        if (static_cast<size_t>(abs(*it)) == id) {
            // if the philosopher is already queued, set his id to negative to force him
            // to wait
            *it = abs(*it) * -1;
            return;
        }
    }
    
    // the philosopher is not in the queue, so add him
    hungryPhils.push_back(static_cast<int>(id));
}

void Table::pickup(unsigned int id) {
    setWaiting(id);
    
    // wait until philosopher can grab forks
    forkLocks[id]->wait();
}

void Table::putdown(unsigned int id) {
    requestedId = id;
}
