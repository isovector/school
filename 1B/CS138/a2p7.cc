#include <iostream>
#include <string>
#include <assert.h>
using namespace std;

struct Node {
    string val;
    Node* next;
    Node* prev;
};

struct Stew {
    Node* first;
    Node* last;
};

Stew initStew (Stew s) {
    s.first = NULL;
    s.last = NULL;
    return s;
}

Node *allocate(string s) {
    Node *ret = new Node;
    ret->val = s;
    return ret;
}

void deallocate(Node *p, bool recursive = false) {
    if (p == NULL) return;

    if (recursive)
        deallocate(p->next, true);
    delete p;
}

Stew nuke(Stew s) {
    deallocate(s.first, true);
    s.first = s.last = NULL;
    return s;
}

bool isEmpty (Stew s) {
    return s.first == NULL;
}

Stew push (string val, Stew s) {
    Node *newone = allocate(val);

    newone->next = s.first;
    if (!isEmpty(s))
        s.first->prev = newone;
    else
        s.last = newone;

    s.first = newone;
    return s;
}

Stew enter (string val, Stew s) { 
    Node *newone = allocate(val);

    newone->prev = s.last;
    if (!isEmpty(s))
        s.last->next = newone;
    else
        s.first = newone;

    s.last = newone;
    return s;
}

Stew pop (Stew s) {
    assert(!isEmpty(s));
    Node *top = s.first;
    s.first = top->next;
    if (!isEmpty(s))
        s.first->prev = NULL;
    if (s.first == NULL || s.first->next == NULL)
        s.last = NULL;
    deallocate(top);

    return s;
 }

string first (Stew s) {
    assert(!isEmpty(s));
    return s.last->val;
}

Stew leave (Stew s) {
    assert(!isEmpty(s));
    Node *bottom = s.last;
    s.last = bottom->prev;
    if (!isEmpty(s))
        s.last->next = NULL;
    if (s.last == NULL || s.last->prev == NULL)
        s.first = NULL;
    deallocate(bottom);

    return s;
}

string top (Stew s) {
    assert(!isEmpty(s));
    return s.first->val;
}

void print (Stew s, char direction = 'a') { 
    if ((direction == 'f' ?: direction == 'r' ?: 0) == 0) {
        cerr << "Error, illegal direction: '" << direction << "`" << endl;
        assert(false);
    }

    Node *pos = (direction == 'f') ? s.first : s.last;
    while (pos) {
        cout << pos->val << endl;
        pos = (direction == 'f') ? pos->next : pos->prev;
    }
}

int main() { 
    Stew s;
    s = initStew(s);
    s = push("hello", s);
    s = push("world", s);
    Node n1 = *s.first;
    Node n2 = *s.last;
    //cout << top(s) << endl;
    s = pop(s);
    s = nuke(s);
 }