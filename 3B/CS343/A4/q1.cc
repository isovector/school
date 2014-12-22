#include <iostream>
using namespace std;

volatile int counter1, counter2
#ifdef PAD
    __attribute__(( aligned (64) )) // align counters on 64-byte boundaries
#endif // PAD
    ;

unsigned int times = 100000000;

_Task Worker1 {
    void main() {
        for ( unsigned int i = 0; i < times; i += 1 ) {
            counter1 += 1;
        } // for
    } // Worker::main
}; // Worker1

_Task Worker2 {
    void main() {
        for ( unsigned int i = 0; i < times; i += 1 ) {
            counter2 += 1;
        } // for
    } // Worker::main
}; // Worker2

void uMain::main() {
    switch ( argc ) {
      case 2:
        times = atoi( argv[1] );
    } // switch

    cout << (void *)&counter1 << " " << (void *)&counter2 << endl;

    uProcessor p;          // add virtual processor
    Worker1 w1;            // create threads
    Worker2 w2;
} // uMain::main
