#ifndef __RANGE_H__
#define __RANGE_H__

#include <iostream>
#include <climits>

// ----------------------------------------------------------------------------
//  Given declarations
// ----------------------------------------------------------------------------

struct Point {
    Point() : x(0), y(0), z(0) { }
    Point(int xx, int yy, int zz) : x(xx), y(yy), z(zz) { } 
    
    int x, y, z;
    
    // This gets the correct component for a given depth
    int operator[](int depth) {
        switch (depth) {
            case 3:  return x;
            case 2:  return y;
            default: return z;
        }
    }
};


void tellEdward(const Point& p);
void tellEdward(int v);




// ----------------------------------------------------------------------------
//  RangeTree declarations
//     these are really generic and their nodes template expand to themselves
// ----------------------------------------------------------------------------

template<int DEPTH> class RangeNode;


// DEPTH is the dimension of the range tree.
template<int DEPTH>
class RangeTree {
    RangeNode<DEPTH>* root;
    
public:
    RangeTree() 
    : root(NULL) {
    }
    
    ~RangeTree() {
        delete root;
    }

    bool add(Point p) {
        if (!root)
            root = new RangeNode<DEPTH>(p, NULL, NULL);
        return root->add(p);
    }
    
    bool search(Point v) {
        if (root)
            return root->search(v);
        return false;
    }
    
    void rangeSearch(Point a, Point b) {
        if (root)
            root->rangeSearch(a, b);
    }
};




// ----------------------------------------------------------------------------
//  Generic RangeNode definition
// ----------------------------------------------------------------------------

template<int DEPTH>
class RangeNode {
    int key;
    RangeTree<DEPTH - 1> tree;  // each node has a Tree of lesser depth

    RangeNode<DEPTH>* left;
    RangeNode<DEPTH>* right;

public:
    RangeNode(Point p, RangeNode<DEPTH> *l = NULL, RangeNode<DEPTH> *r = NULL)
    : key(p[DEPTH]), left(l), right(r) {
    }
    
    ~RangeNode() {
        delete left;
        delete right;
    }

    bool add(Point p) {
        int v = p[DEPTH];
        
        if (key == v)
            return tree.add(p);
        else if (v > key) {
            if (!right)
                right = new RangeNode<DEPTH>(p);
            return right->add(p);
        }
        else {
            if (!left)
                left = new RangeNode<DEPTH>(p);
            return left->add(p);
        }
        
        return true;
    }
    
    bool search(Point p) {
        int v = p[DEPTH];

        if (key == v)
            return tree.search(p);
        else if (v > key && right)
            return right->search(p);
        else if (v < key && left)
            return left->search(p);

        return false;
    }
    
    void rangeSearch(Point a, Point b) {
        int lo = a[DEPTH];
        int hi = b[DEPTH];
        
        if (lo <= key && hi >= key)
            tree.rangeSearch(a, b);
        
        if (left && lo < key)
            left->rangeSearch(a, b);
        if (right && hi > key)
            right->rangeSearch(a, b);
    }
};




// ----------------------------------------------------------------------------
//  Specialized RangeNode definition
// ----------------------------------------------------------------------------

// We define DEPTH here for symmetry with the generic case
#define DEPTH 1

template<>
class RangeNode<DEPTH> {
    int key;
    Point point;    // the base case has a Point as node data

    RangeNode<DEPTH>* left;
    RangeNode<DEPTH>* right;

public:
    RangeNode(Point p, RangeNode<DEPTH> *l = NULL, RangeNode<DEPTH> *r = NULL)
    : key(p[DEPTH]), left(l), right(r) {
        point = p;
    }
    
    ~RangeNode() {
        delete left;
        delete right;
    }

    bool add(Point p) {
        int v = p[DEPTH];
        
        if (key == v)
            return false;
        else if (v > key) {
            if (!right)
                right = new RangeNode<DEPTH>(p);
            return right->add(p);
        }
        else {
            if (!left)
                left = new RangeNode<DEPTH>(p);
            return left->add(p);
        }
        
        return true;
    }
    
    bool search(Point p) {
        int v = p[DEPTH];
        
        if (key == v)
            return true;
        else if (v > key && right)
            return right->search(p);
        else if (v < key && left)
            return left->search(p);

        return false;
    }
    
    void rangeSearch(Point a, Point b) {
        int lo = a[DEPTH];
        int hi = b[DEPTH];
        
        if (lo <= key && hi >= key)
            tellEdward(point);
        
        if (left && lo < key)
            left->rangeSearch(a, b);
        if (right && hi > key)
            right->rangeSearch(a, b);
    }
};

#undef DEPTH




// ----------------------------------------------------------------------------
//  Interface wrappers macros
//    The assignment's required classes don't match our RangeTrees, so we need
//    some wrappers. These differ only in name and in declaration of rangeSearch.
//    Sounds like a great use for macros :)
// ----------------------------------------------------------------------------

// The implementation of RangeTree#D, with an open
// definition for rangeSearch, so that the arguments
// and implementation can be written directly after.
#define RANGE_TREE(d)                       \
bool RangeTree##d##D::add(Point p) {        \
    return tree.add(p);                     \
}                                           \
                                            \
bool RangeTree##d##D::search(Point p) {     \
    return tree.search(p);                  \
}                                           \
                                            \
void RangeTree##d##D::rangeSearch



// The start of the class declaration
#define RANGE_TREE_START(d)                 \
class RangeTree##d##D {                     \
        RangeTree<d> tree;                  \
                                            \
public:                                     \
    bool add(Point p);                      \
    bool search(Point p);                   \



// The end of the class declaration
#define RANGE_TREE_END }




// ----------------------------------------------------------------------------
//  Interface wrapper declarations
// ----------------------------------------------------------------------------

RANGE_TREE_START(1)
    void rangeSearch(const int zlo, const int zhi);
RANGE_TREE_END;

    
RANGE_TREE_START(2)
    void rangeSearch(const int ylo, const int yhi, const int zlo, const int zhi);
RANGE_TREE_END;

    
RANGE_TREE_START(3)
    void rangeSearch(const int xlo, const int xhi, const int ylo, const int yhi, const int zlo, const int zhi);
RANGE_TREE_END;

    
#endif
