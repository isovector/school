#include <cassert>
#include <iostream>
#include "BST.h"
using namespace std;


BSTNode::BSTNode(int v, BSTNode* l, BSTNode* r) :
    value(v), left(l), right(r) {}

BSTNode::~BSTNode()
{
    delete left;
    delete right;
}


bool BSTNode::add(int v)
{
    if (value == v)
        return false;
    else if (v > value)
        if (right)
            return right->add(v);
        else
            right = new BSTNode(v);
    else
        if (left)
            return left->add(v);
        else
            left = new BSTNode(v);
    return true;
}

bool BSTNode::search(int v)
{
    if (value == v)
        return true;
    else if (v > value && right)
        return right->search(v);
    else if (v < value && left)
        return left->search(v);
    else
        return false;
}

void BSTNode::rangeSearch(int lo, int hi) {
    if (lo <= value && hi >= value)
        tellEdward(value);
    
    if (left && lo < value)
        left->rangeSearch(lo, hi);
    if (right && hi > value)
        right->rangeSearch(lo, hi);
}


BinarySearchTree::BinarySearchTree() : 
    root(NULL) {}

BinarySearchTree::~BinarySearchTree()
{
    delete root;
}

bool BinarySearchTree::add(int v)
{
    if (!root)
        root = new BSTNode(v, NULL, NULL);
    else
        return root->add(v);
    return true;
}

bool BinarySearchTree::search(int v)
{
    if (!root)
        return false;
    else
        return root->search(v);
}

void BinarySearchTree::rangeSearch(int lo, int hi) {
    if (root)
        root->rangeSearch(lo, hi);
}



#ifndef __TESTER__
/**
    tellEdward p -> (state)
    Outputs a point or an integer */
void tellEdward(const Point& p) {
    cout << "(" << p.x << " " << p.y << " " << p.z << ")" << endl;
}
void tellEdward(int v) {
    cout << v << endl;
}

/** Testing search + add in a binary search tree */
void testsimpleBST()
{
    BinarySearchTree x;
    assert(x.add(1));
    assert(!x.add(1));
    assert(x.add(2));
    assert(x.add(3));
    assert(x.add(4)); 
    assert(!x.add(2));
    assert(x.add(-1));

    assert(x.search(1));
    assert(x.search(2));
    assert(x.search(3));
    assert(x.search(4));
    assert(x.search(-1));
}

/** Testing Simple RangeSearch
    (this code intentionally left guarded out) */
#if DONE_RANGESEARCH
void testsimpleRS()
{
    BinarySearchTree x;
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

int main( )
{ 
    BinarySearchTree bst;
    for (int i = 0; i < 11; i++)
        bst.add(11 - i);
        
    bst.rangeSearch(4, 15);

    return 0;
}
#endif
