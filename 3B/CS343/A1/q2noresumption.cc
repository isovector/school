#include <iostream>
#include <cstdlib>          // access: rand, srand
#include <unistd.h>         // access: getpid
using namespace std;

// base functor class for fixups
struct fixup {
    virtual void operator ()(int &i, fixup* fix) = 0;
};

void f(int &i, fixup* fix);
void g(int &i, fixup* fix);

// implementation of f's resume handler declared
// as variable fix_f
struct __fixup_functor_for_f : public fixup {
    virtual void operator ()(int &i, fixup* fix) {
        cout << "f handler, i:" << i << endl;
        i -= 7;
        if (rand() % 7 == 0) {
            g(i, fix);
        }
    }
} fix_f;

// implementation of g's resume handler declared
// as variable fix_g
struct __fixup_functor_for_g : public fixup {
    virtual void operator ()(int &i, fixup* fix) {
        cout << "g handler, i:" << i << endl;
        i -= 5;
        if (rand() % 5 == 0) {
            f(i, fix);
        }
    }
} fix_g;

void f(int &i, fixup* fix) {
    cout << "f " << i << endl;
    
    if (rand() % 5 == 0) {
        // this throw is always caught by fix_f
        // so we hardcode it
        fix_f(i, fix);
    }
    
    if (rand() % 7 == 0) {
        g(i, &fix_f);
    }
    
    if (0 < i) {
        f(i, fix);
    }
}

void g(int &i, fixup* fix) {
    cout << "g " << i << endl;
    if (rand() % 7 == 0) {
        // this throw is always caught by fix_g
        // so we hardcode it
        fix_g(i, fix);
    }
    
    if (rand() % 5 == 0) {
        f(i, &fix_g);
    }
    
    if (0 < i) {
        g(i, fix);
    }
}

int main(int argc, char *argv[]) {
    int times = 25, seed = getpid();
    
    if (argc >= 2) {
        times = atoi(argv[1]);             // control recursion depth
    }
    
    if (argc == 3) {
        seed  = atoi(argv[2]);             // allow repeatable experiment
    }
    
    srand(seed);                                        // fixed or random seed
    f(times, &fix_f);
    
    return 0;
}
