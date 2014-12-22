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
} lastSuccess;

void rtn1(int i) {
    for (int j = 0; j < times; j += 1) {
        if (rand() % 100000000 == 42) {
            lastSuccess = Success(j);
            return;
        }
    }
}

void rtn2(int i) {
    for (int j = 0; -j < times; j -= 1) {
        if (rand() % 100000000 == 42) {
            lastSuccess = Success(j);
            return;
        }
    }
}

void g(int i) {
    for (int j = 0; j < times && lastSuccess; j += 1) {
        if (rand() % 2 == 0) {
            rtn1(i);
        }
        else {
            rtn2(i);
        }
    }
    
    if (!lastSuccess)
    {
        return;
    }
    
    if (i % 2) {
        rtn2(i);
    }
    
    if (lastSuccess)
    {
        rtn1(i);
    }
}

void f(int i) {
    for (int j = 0; j < times && lastSuccess; j += 1) {
        g(i);
    }
    
    if (!lastSuccess) {
        return;
    }
    
    if (i % 2) {
        g(i);
    }
    
    if (lastSuccess) {
        g(i);
    }
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
    
    f(3);
    if (lastSuccess) {
        cout << "seed:" << seed << " times:" << times << " complete" << endl;
    } else {
        cout << "seed:" << seed << " times:" << times << " rc:" << lastSuccess.error << endl;
    }
}
