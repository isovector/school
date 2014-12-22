#include <iostream>
#include <fstream>
#include <algorithm>
using namespace std;

#include "q3mergesort.h"

unsigned int uDefaultStackSize() {
    return 512 * 1000;
}

// Implements the first mode of the assignment
void mode1(istream &in, ostream &out) {
    size_t elementCount;
    
    // Keep reading array counts as long as we can
    while (in >> elementCount) {
        int *values = new int[elementCount];
        for (size_t i = 0; i < elementCount; i++) {
            in >> values[i];
            out << values[i] << " ";
        }
        out << endl;
        
        {
            // Sort the values and wait until it's finished
            Mergesort<int> sort(values, 0, elementCount, 0);
        }
        
        // Output the sorted values
        for (size_t i = 0; i < elementCount; i++) {
            out << values[i] << " ";
        }
        
        out << endl << endl;
        delete values;
    }
}

// Implements mode two
void mode2(istream &in, int depth) {
    size_t arraySize;
    in >> arraySize;

    // Initialize our array
    int *values = new int[arraySize];
    for (size_t i = 0; i < arraySize; ++i) {
        values[i] = arraySize - i;
    }
    
    {
        // Rock that array, girrrrrrl!
        uProcessor p[depth - 1] __attribute__((unused));
        Mergesort<int> sort(values, 0, arraySize, depth);
    }
    
    delete values;
}

void uMain::main() {
    if (argc >= 2) {
        ifstream in(argv[1]);
        int depth = 0;

        if (argc == 2) {
            mode1(in, cout);
        } else if (*argv[2] != '-') {
            ofstream out(argv[2]);
            mode1(in, out);
        } else if ((depth = atoi(argv[2] + 1)) && depth > 0) {
            mode2(in, depth);
        } else {
          goto usage;
        }
        
        return;
    }
    
  usage:
    cout << "Usage: ./mergesort unsorted-file [ sorted-file | -depth (> 0) ]" << endl;
}
