#include "MPRNG.h"
#include "q2reindeer.h"

Reindeer::Reindeer(size_t id, Workshop &wrk, Printer &prt, size_t numDeliveries)
    : id(id), workshop(wrk), printer(prt), numDeliveries(numDeliveries)
{ }

void Reindeer::main() {
    #define print(x) (print(Printer::x))
    
    yield(mprand(0, 10));
    print(Starting);
    
    for (size_t i = 0; i < numDeliveries; ++i) {
        yield(mprand(0, 3));
        print(OnVacation);
        yield(mprand(0, 5));
        print(CheckingIn);
        
        workshop.deliver(id);
        print(DeliveringToys);
        yield(mprand(0, 5));
        workshop.doneDelivering(id);
        print(DoneDelivering);
    }

    print(Finished);
    
    #undef print
}

void Reindeer::print(Printer::States state) const {
    printer.print(id, state);
}
