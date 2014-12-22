#ifndef __RANGE_H__
#define __RANGE_H__

#include <ostream>


struct Point {
    int x, y, z;
};


void tellEdward(const Point& p);
void tellEdward(int v);


class BSTNode {
private:
    int value;    
    BSTNode* left;
    BSTNode* right;

public:
    BSTNode(int v, BSTNode *l = NULL, BSTNode *r = NULL);
    ~BSTNode();

    bool add(int v);
    bool search(int v);
    void rangeSearch(int lo, int hi);

};

class BinarySearchTree {
private:
    BSTNode* root;
public:
    BinarySearchTree();
    ~BinarySearchTree();

    bool add(int v);
    bool search(int v);
    void rangeSearch(int lo, int hi);
};

#endif
