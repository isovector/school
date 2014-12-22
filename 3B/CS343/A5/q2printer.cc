#include "q2printer.h"
#include <sstream>
#include <iostream>
using namespace std;

Printer::StateNum::StateNum(States state) :
    state(state), hasNums(false), numA(0), numB(0)
{ }

Printer::StateNum::StateNum(States state, size_t numA, size_t numB) :
    state(state), hasNums(true), numA(numA), numB(numB)
{ }

// we do not define operator !=, so you should always use !(a==b)
bool Printer::StateNum::operator==(States other) {
    return state == other;
}

// we do not define operator !=, so you should always use !(a==b)
bool Printer::StateNum::operator==(const StateNum &other) {
    return state == other.state && numA == other.numA && numB == other.numB;
}

// Get a string representation of this statenum
string Printer::StateNum::getString() const {
    stringstream stream;
    
    stream << static_cast<char>(state);
    if (hasNums) {
        stream << numA << "," << numB;
    }
    
    return stream.str();
}

// ------------------------------------

Printer::Printer(unsigned int NoOfPhil) {
    for (size_t i = 0; i < NoOfPhil; ++i) {
        states.push_back(StateNum(None));
    }
    
    preamble(NoOfPhil);
}

// Output the header of the printer
void Printer::preamble(size_t numPhils) const {
    for (size_t i = 0; i < numPhils; ++i) {
        cout << "Phil" << i << "\t";
    }
    cout << endl;
    
    for (size_t i = 0; i < numPhils; ++i) {
        cout << "******\t";
    }
    cout << endl;
}
    
void Printer::print(unsigned int id, States state) {
    printRaw(id, StateNum(state));
}

void Printer::print(unsigned int id, States state, unsigned int bite, unsigned int noodles) {
    printRaw(id, StateNum(state, bite, noodles));
}

// u++ gets pissy if you make private and public functions with the saame name...
void Printer::printRaw(size_t id, StateNum state) {
    // != operator is not defined on StateNum, thus this ugly expression
    // if we are reassigning a state (which didn't use to be None)
    // OR if we are finished, flush the printer buffer
    if ((!(states[id] == state) && !(states[id] == None)) || state == Finished) {
        flush();
    }
    
    if (state == Finished) {
        // Write out finished lines
        for (size_t i = 0; i < states.size(); ++i) {
            cout << (id == i ? "F" : "...") << "\t";
        }
        
        cout << endl;
    } else {
        states[id] = state;
    }
}

// Output the printer buffer
void Printer::flush() {
    bool isDirty = false;
    for (vector<StateNum>::iterator it = states.begin(); it != states.end(); ++it) {
        // operator != is not defined
        if (!(*it == None)) {
            isDirty = true;
            break;
        }
    }
    
    // Nothing has changed, so no need to print
    if (!isDirty) {
        return;
    }
    
    for (vector<StateNum>::iterator it = states.begin(); it != states.end(); ++it) {
        cout << it->getString() << "\t";
        
        // Set state of this element to None
        *it = StateNum(None);
    }
    
    cout << endl;
}
