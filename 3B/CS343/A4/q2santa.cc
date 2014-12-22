#include "MPRNG.h"
#include "q2santa.h"

Santa::Santa(Workshop &wrk, Printer &prt) 
    : workshop(wrk), printer(prt) 
{ }

void Santa::main() {
    #define print(x) (print(Printer::x))
    
    yield(mprand(0, 10));
    print(Starting);
    
    TheGoodLife: while (true) {
        yield(mprand(0, 3));
        print(Napping);
        Workshop::Status status = workshop.sleep();
        
        switch (status) {
            case Workshop::Done:
                break TheGoodLife;
            
            case Workshop::Consulting: {
                yield(mprand(0, 3));
                print(DoneConsulting);
            } break;
            
            case Workshop::Delivery: {
                yield(mprand(0, 5));
                print(DoneDelivering);
            } break;
        }
    }
    
    print(Finished);
    
    #undef print
}

void Santa::print(Printer::States state) const {
    static const size_t SANTA = 0;
    printer.print(SANTA, state);
}
