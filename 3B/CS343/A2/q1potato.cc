#include "q1potato.h"

#include <iostream>
using namespace std;

Potato::Potato(unsigned int maxTicks) {
}

void Potato::reset(unsigned int maxTicks) {
    ticks = rand() % maxTicks + 1;
    cout << "  POTATO will go off after " << ticks << " toss" << (ticks != 1 ? "es" : "") << endl; 
}

bool Potato::countdown() {
    --ticks;
    
    return ticks == 0;
}