#include "MPRNG.h"
#include "q2elf.h"

Elf::Elf(size_t id, Workshop &wrk, Printer &prt, size_t numConsultations)
    : id(id), workshop(wrk), printer(prt), numConsultations(numConsultations)
{ }

void Elf::main() {
    #define print(x) (print(Printer::x))
    
    yield(mprand(0, 10));
    print(Starting);
    
    for (size_t i = 0; i < numConsultations; ++i) {
        yield(mprand(0, 3));
        print(Working);
        yield(mprand(0, 5));
        print(NeedHelp);
        
        if (!workshop.consult(id)) {
            print(ConsultingFailed);
            break;
        }
        
        print(Consulting);
        yield(mprand(0, 3));
        workshop.doneConsulting(id);
        print(DoneConsulting);
    }
    
    workshop.termination(id);
    print(Finished);
    
    #undef print
}

void Elf::print(Printer::States state) const {
    printer.print(id, state);
}
