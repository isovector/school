#include <iostream>
#include <string>
#include <assert.h>
#include <set>
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
    ret->next = NULL;
    ret->prev = NULL;
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

    if (s.first == NULL) {
        s.first = s.last = newone;
        return s;
    }

    newone->next = s.first;
    s.first->prev = newone;
    s.first = newone;
    return s;
}

Stew pop (Stew s) {
    assert(!isEmpty(s));

    if (s.first == s.last) {
        deallocate(s.first);
        s.first = s.last = NULL;
        return s;
    }

    Node *top = s.first;
    s.first = top->next;
    s.first->prev = NULL;
    deallocate(top);

    return s;
 }

Stew enter (string val, Stew s) {
    Node *newone = allocate(val);

    if (s.last == NULL) {
        s.first = s.last = newone;
        return s;
    }

    newone->prev = s.last;
    s.last->next = newone;
    s.last = newone;
    return s;
}

Stew leave (Stew s) {
    assert(!isEmpty(s));

    if (s.first == s.last) {
        deallocate(s.first);
        s.first = s.last = NULL;
        return s;
    }

    Node *top = s.last;
    s.last = top->prev;
    s.last->next = NULL;
    deallocate(top);

    return s;
 }


string first (Stew s) {
    assert(!isEmpty(s));
    return s.last->val;
}

string top (Stew s) {
    assert(!isEmpty(s));
    return s.first->val;
}

void print (Stew s, char direction = 'a') { 
    set<Node*> closed;

    if ((direction == 'f' ?: direction == 'r' ?: 0) == 0) {
        cerr << "Error, illegal direction: '" << direction << "`" << endl;
        assert(false);
    }

    Node *pos = (direction == 'f') ? s.first : s.last;
    while (pos) {
        if (!closed.insert(pos).second) {
            break;
        }
        cout << pos->val << endl;
        pos = (direction == 'f') ? pos->next : pos->prev;
    }
}

int main() { 
    Stew s;
    s = initStew(s);
    s = enter("hello", s);
    s = enter("world", s);
    s = enter("sup", s);
    s = enter("bitch", s);
    print(s, 'f');
    s = leave(s);
    s = leave(s);
    s = leave(s);
    s = enter("2", s);
    s = leave(s);
    s = enter("5", s);
    s = leave(s);
    s = leave(s);

    //Node n1 = *s.first;
    //Node n2 = *s.last;
    //cout << top(s) << endl;
    //s = pop(s);
    s = nuke(s);
 }