#include <iostream>
#include <cstdlib>          // access: rand, srand
#include <unistd.h>         // access: getpid
using namespace std;

int times = 1000;

struct Success {
    int error;
    bool isSuccess;
    
    Success() :           error(0),       isSuccess(true) { }
    Success(int error) :  error(error),   isSuccess(false) { }
    
    operator bool() const {
        return isSuccess;
    }
};

Success rtn1(int i) {
    Success e;
    
    int j = 0;
    for ( ; j < times && e; j += 1) {
        if (rand() % 100000000 == 42) {
            e = j;
        }
    }
    
    return e;
}

Success rtn2(int i) {
    Success e;
    
    int j = 0;
    for ( ; -j < times && e; j -= 1) {
        if (rand() % 100000000 == 42) {
            e = j;
        }
    }
    
    return e;
}

Success g(int i) {
    Success e;
    
    for (int j = 0; j < times && e; j += 1) {
        if (rand() % 2 == 0) {
            e = rtn1(i);
        }
        else {
            e = rtn2(i);
        }
    }
    
    if (i % 2 && e) {
        e = rtn2(i);
    }
    
    if (e) {
        e = rtn1(i);
    }
    
    return e;
}

Success f(int i) {
    Success e;
    
    for (int j = 0; j < times && e; j += 1) {
        e = g(i);
    }
    
    if (i % 2 && e) {
        e = g(i);
    }
    
    if (e) {
        e = g(i);
    }
    
    return e;
}

int main(int argc, char *argv[]) {
    int seed = getpid();
    
    if (argc >= 2) {
        seed = atoi(argv[1]);
    }
    
    srand(seed);
    
    if (argc == 3) {
        times = atoi(argv[2]);
    }
    
    Success e = f(3);
    if (e) {
        cout << "seed:" << seed << " times:" << times << " complete" << endl;
    } else { 
        cout << "seed:" << seed << " times:" << times << " rc:" << e.error << endl;
    }
}
