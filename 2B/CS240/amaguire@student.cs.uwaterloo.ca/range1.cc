#include <cassert>
#include <iostream>
#include "range1.h"
using namespace std;

// ----------------------------------------------------------------------------
//  Interface wrapper definitions
// ----------------------------------------------------------------------------

RANGE_TREE(1)(const int zlo, const int zhi) {
    tree.rangeSearch(Point(INT_MIN, INT_MIN, zlo), Point(INT_MAX, INT_MAX, zhi));
}

    
RANGE_TREE(2)(const int ylo, const int yhi, const int zlo, const int zhi) {
    tree.rangeSearch(Point(INT_MIN, ylo, zlo), Point(INT_MAX, yhi, zhi));
}

    
RANGE_TREE(3)(const int xlo, const int xhi, const int ylo, const int yhi, const int zlo, const int zhi) {
   tree.rangeSearch(Point(INT_MIN, ylo, zlo), Point(INT_MAX, yhi, zhi));
}




// ----------------------------------------------------------------------------
//  This ain't my code!
// ----------------------------------------------------------------------------

#ifndef __TESTER__
void tellEdward(const Point& p) {
    cout << "(" << p.x << " " << p.y << " " << p.z << ")" << endl;
}
void tellEdward(int v) {
    cout << v << endl;
}

#if DONE_RANGESEARCH
void testsimpleRS() {
    RangeTree x;
    assert(x.add(1));
    assert(!x.add(1));
    assert(x.add(2));
    assert(x.add(3));
    assert(x.add(4)); 
    assert(!x.add(2));
    assert(x.add(-1));

    /** Should display 2 <newline> 3 <newline> 4 */
    x.rangeSearch(2,4);
}
#endif

int main() { 
    RangeTree2D bst;
    for (int i = 0; i < 11; i++)
        bst.add(Point(i, i, i));
        
    bst.rangeSearch(1, 5, 5, 10);

    return 0;
}
#endif
