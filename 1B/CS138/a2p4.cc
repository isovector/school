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
    }

    p2->next = p1;
    p1->next = (Node*)NULL;
    return p2;
}

Node *makePairList(string s1, string s2) {
    Node *p = new Node[2];
    if (s1 < s2) {
        p[0].val = s1;
        p[0].next = &p[1];
        p[1].val = s2;
        return &p[0];
    }

    p[1].val = s1;
    p[1].next = &p[0];
    p[0].val = s2;
    return &p[1];
}

void printReverseRecursive() {
    string result;
    if (!(cin >> result)) return;
    printReverseRecursive();
    cout << result << endl;
}

int main() { printReverseRecursive(); return 0; }
