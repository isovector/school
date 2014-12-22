#ifndef Q2WORKSHOP_H
#define Q2WORKSHOP_H

#include "q2printer.h"
#include <deque>
using namespace std;

_Monitor Workshop {
    Printer &printer;
    
    uCondition theSlumberOfDarkThings;
    uCondition theImpatienceOfBeasts;
    uCondition theBusyElves;
    std::deque<size_t> elvesWaiting;
    std::deque<size_t> reindeerWaiting;
    
    size_t N, E, D;
    size_t n, elvesRemaining;
    
  public:
    enum Status { Consulting, Delivery, Done };
    
    Workshop(Printer &prt, size_t N, size_t E, size_t D); // printer, bound, elves, reindeer delivery
    Status sleep();                               // santa calls to nap; when Santa wakes status of next action
    void deliver(size_t id);              // reindeer call to deliver toys
    bool consult(size_t id);              // elves call to consult Santa,
                                                  //   true => consultation successful, false => consultation failed
    void doneConsulting(size_t id);       // block Santa/elves until meeting over
    void doneDelivering(size_t id);       // block Santa/reindeer until all toys are delivered
    void termination(size_t id);          // elves call to indicate termination
};

#endif
