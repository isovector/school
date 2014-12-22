#include "q2table.h"
#include "q2printer.h"
#include "q2philosopher.h"

#include <vector>
#include <iostream>
using namespace std;

MPRNG mprand;

void usage() {
    cout << "Usage: ./phil [ P [ N [ S ] ] ]" << endl;
    exit(1);
}

void uMain::main() {
    if (argc > 4) {
        usage();
    }
    
    int Pval = argc > 1 ? atoi(argv[1]) : 5;
    int Nval = argc > 2 ? atoi(argv[2]) : 30;
    int S =  argc > 3 ? atoi(argv[3]) : 1337;
    
    if (Pval <= 1 || Nval <= 0 || S <= 0) {
        usage();
    }

    size_t P = static_cast<size_t>(Pval);
    size_t N = static_cast<size_t>(Nval);
    
    mprand.seed(S);
    
    Printer printer(P);
    Table table(P, printer);
    
    vector<Philosopher*> philosophers;
    for (size_t i = 0; i < P; ++i) {
        philosophers.push_back(new Philosopher(i, N, table, printer));
    }
    
    // join all philosophers
    for (size_t i = 0; i < P; ++i) {
        delete philosophers[i];
    }
    
    cout << "***********************\nPhilosophers terminated" << endl;
}
