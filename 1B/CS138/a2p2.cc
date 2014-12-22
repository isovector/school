#include <iostream>
#include <string>
#include <assert.h>
using namespace std;

struct Node {
    string val;
    Node* next;
};

void printPairInOrder(Node *p1, Node *p2) {
    assert(p1);
    assert(p2);

    if (p1->val < p2->val)
        cout << p1->val << endl << p2->val << endl;
    else 
        cout << p2->val << endl << p1->val << endl;
}

Node *sortPair(Node *p1, Node *p2) {
    assert(p1);
    assert(p2);

    if (p1->val < p2->val) {
        p1->next = p2;
        p2->next = (Node*)NULL;
        return p1;
    } else {
        p2->next = p1;
        p1->next = (Node*)NULL;
        return p2;
    }
}

int main() { return 0; }