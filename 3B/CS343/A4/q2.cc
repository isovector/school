#include "MPRNG.h"

#include "q2santa.h"
#include "q2elf.h"
#include "q2reindeer.h"

#include <vector>
using namespace std;

MPRNG mprand;

void uMain::main() {
    size_t N = 3;
    size_t E = 3;
    size_t C = 3;
    size_t D = 3;
    
    Printer printer(E);
    Workshop workshop(printer, N, E, D);
    Santa santa(workshop, printer);
    
    vector<Elf*> elves;
    vector<Reindeer*> reindeer;
    for (size_t i = 0; i < E; ++i) {
        elves.push_back(new Elf(1 + i, workshop, printer, C));
    }
    
    for (size_t i = 0; i < Reindeer::MAX_NUM_REINDEER; ++i) {
        reindeer.push_back(new Reindeer(1 + E + i, workshop, printer, D));
    }
    
    for (int i = E - 1; i >= 0; --i) {
        delete elves[i];
        elves.pop_back();
    }
    
    for (int i = Reindeer::MAX_NUM_REINDEER - 1; i >= 0; --i) {
        delete reindeer[i];
        reindeer.pop_back();
    }
}
