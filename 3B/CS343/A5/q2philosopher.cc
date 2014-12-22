#include "q2philosopher.h"

// Helper macro to simplify printer calls.
// Not a function to avoid needing to write one for each type of vararg
#define print(state, varargs...) printer.print(id, Printer::state, ##varargs)
                            // ,##varargs is only a comma if there are varargs

Philosopher::Philosopher(unsigned int id, unsigned int noodles, Table &table, Printer &prt) :
    id(id), noodles(noodles), table(table), printer(prt)
{
}

void Philosopher::main() {
    while (noodles != 0) {
        print(Hungry);
        yield(mprand(0, 4));
        
        table.pickup(id);

        size_t toEat = mprand(1, 5);
        if (toEat > noodles) {
            toEat = noodles;
        }
        
        print(Eating, toEat, noodles - toEat);
        yield(mprand(0, 4));
        
        table.putdown(id);
        
        noodles -= toEat;
        if (noodles == 0) {
            break;
        }
        
        print(Thinking);
        yield(mprand(0, 19));
    }
    
    print(Finished);
}
