#include <iostream>
using namespace std;

#include "q2printer.h"

Printer::Printer(const size_t MAX_NUM_ELVES)
{
    static const size_t MAX_NUM_REINDEER = 5; // I don't feel like sorting out the dependencies
    
    for (size_t i = 0; i < 1 + MAX_NUM_ELVES + MAX_NUM_REINDEER; ++i) {
        states.push_back(StateNum(None, 0));
    }
    
    preamble(MAX_NUM_ELVES);
}

void Printer::print(size_t id, States state) {
    print(id, state, 0);
}

void Printer::print(size_t id, States state, size_t numBlocked) {
    if (states[id].state != None || state == Finished) {
        flush();
    }

    if (state == Finished) {
        for (size_t i = 0; i < states.size(); ++i) {
            cout << (id == i ? "F" : "...") << "\t";
        }
        
        cout << endl;
    } else {
        states[id] = StateNum(state, numBlocked);
    }
}

void Printer::preamble(size_t elves) const {
    cout << "Sa";
    for (size_t i = 0; i < elves; ++i) {
        cout << "\tE" << (i + 1);
    }
    
    for (size_t i = 1 + elves; i < states.size(); ++i) {
        cout << "\tR" << (i + 1);
    }
    
    cout << endl;
    
    for (size_t i = 0; i < states.size(); ++i) {
        cout << "--\t";
    }
    
    cout << endl;
}

void Printer::flush() {
    bool isDirty = false;
    for (vector<StateNum>::iterator it = states.begin(); it != states.end(); ++it) {
        if (it->state != None) {
            isDirty = true;
            break;
        }
    }
    
    if (!isDirty) {
        return;
    }
    
    for (vector<StateNum>::iterator it = states.begin(); it != states.end(); ++it) {
        cout << static_cast<char>(it->state);
        if (it->num != 0) {
            cout << " " << it->num;
        }
        
        cout << "\t";
        
        *it = StateNum(None, 0);
    }
    
    cout << endl;
}
