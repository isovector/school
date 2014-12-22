#include <iostream>
#include <fstream>
#include <sys/types.h>
#include <dirent.h>
#include <vector>
using namespace std;

#include "uSemaphore.h"

#include "q1sortmapper.h"
#include "q1sortreducer.h"

void usage() {
    cout << "Usage: ./wordcount input-directory [ num-reducers ( > 0 ) [ queue-length ( > 0 ) [ sort-buffer-size ( >= 0 ) ] ] ]" << endl;
    exit(1);
}

void uMain::main() {
    if (argc < 2 || argc > 5) {
        usage();
    }
    
    string dirname = argv[1];
    int numReducers = argc > 2 ? atoi(argv[2]) : 4;
    int queueLength = argc > 3 ? atoi(argv[3]) : 16;
    int bufferSize =  argc > 4 ? atoi(argv[4]) : 1;
    
    if (numReducers <= 0 || queueLength <= 0 || bufferSize < 0) {
        usage();
    }
    
    DIR *dir = opendir(dirname.c_str());
    if (!dir) {
        cout << "Error! Cannot open directory \"" << dirname << "\"" << endl;
        usage();
    }
    
    // Append a trailing / to the path if it doesn't exist already
    if (dirname[dirname.size() - 1] != '/') {
        dirname += "/";
    }
    
    vector<Mapper*> mappers;
    vector<Reducer*> reducers;
    uSemaphore signal(0);
    
    dirent *dp;
    // This side effect is unfortunate, but the author thinks it is more readable
    // than the alternative.
    while ((dp = readdir(dir)) != NULL) {
        if (dp->d_type != DT_REG) {
            continue;
        }
        
        if (bufferSize == 0) {
            mappers.push_back(new Mapper(dirname + dp->d_name, queueLength, &signal));
        } else {
            mappers.push_back(new SortMapper(dirname + dp->d_name, queueLength, bufferSize, &signal));
        }
    }
    closedir(dir);
    
    // size_t is my jam
    for (size_t i = 0; i < static_cast<size_t>(numReducers); ++i) {
        if (bufferSize == 0) {
            reducers.push_back(new Reducer(i, numReducers, &signal, mappers));
        } else {
            reducers.push_back(new SortReducer(i, numReducers, &signal, mappers));
        }
    }
    
    for (vector<Reducer*>::iterator it = reducers.begin(); it != reducers.end(); ++it) {
        delete *it;
    }
    
    for (vector<Mapper*>::iterator it = mappers.begin(); it != mappers.end(); ++it) {
        delete *it;
    }

    cout << "Finished! Semaphore counter: " << signal.counter() << endl;
}
