#include <iostream>
#include <string>
#include <assert.h>
#include <cstdio>
#include <cstdlib>
#include <signal.h>
using namespace std;

void catch_segfault(int sig_num) {
    exit(0);
}

// a node in a queue
struct Qnode {
    string val;
    Qnode *next;
};

// a queue
struct Queue {
    Qnode *first;
    Qnode *last;
};

// a priority queue
struct PQnode {
    int priority;
    PQnode *next;
    Queue q;
};

typedef PQnode* PQ;


void initPQ(PQ& pq) {
    signal(SIGSEGV, catch_segfault);
    pq = NULL;
}

// count the number of non-empty priorities in the queue
int numPriorities(PQ pq) {
    int count = 0;
    while (pq) {
        // if it has data, we can count it
        if (pq->q.first) count++;
        pq = pq->next;
    }
    return count;
}

bool isEmptyPQ(PQ pq) {
    return numPriorities(pq) == 0;
}

// allocates a new PQndoe
PQnode *makePQ(int priority) {
    PQnode *gen = new PQnode;
    gen->priority = priority;
    gen->next = NULL;
    return gen;
}

// enqueue a string
void enterPQ(PQ& pq, string val, int priority) {
    PQnode *node = pq;

    bool first = true;
    if (isEmptyPQ(pq)) {
        node = makePQ(priority);
    } else {
        while (node) { // breadth-wide search
            // this node´s priority is less than ours
            if (node->priority < priority)
                // this node is either the last or the last one < our priority
                if (!node->next || node->next->priority > priority) {
                    if (!node->next)
                        first = false;
                    PQnode *gen = makePQ(priority);
                    // insert a priority node here
                    gen->next = node->next;
                    node->next = gen;
                    node = gen;
                    break;
                // traverse
                } else 
                    node = node->next;
            // our priority is < everything already in the queue
            else if (node->priority > priority) {
                PQnode *gen = makePQ(priority);
                gen->next = node;
                node = gen;
                first = true;
                break;
            // else this is the right node
            } else break;
            first = false;
        }
    }

    // move to the end of our node
    Qnode *last = node->q.first;
    while (last && last->next)
        last = last->next;

    // allocate a new queue frame
    Qnode *frame = new Qnode;
    frame->val = val;
    frame->next = NULL;

    // if the priority is not empty
    if (last)
        last->next = frame;
    // otherwise we need to 
    else 
        node->q.first = frame;
    node->q.last = frame;

    // if this is the first priority, we update our struct pointer
    if (first) 
        pq = node;

    // ensure no circular links were created
    // mostly debugging
    assert(node->next != node);
}

// return the first element in the PQ
string firstPQ(PQ pq) {
    assert(!isEmptyPQ(pq));

    // breadth traversal
    while (pq) {
        // if we have data
        if (pq->q.first)
            return pq->q.first->val;
        pq = pq->next;
    }

    return "";
}

// pop the first elem
void leavePQ(PQ& pq) {
    assert(!isEmptyPQ(pq));

    PQnode *node = pq;
    while (node) {
        // if this node isn´t empty
        if (node->q.first) {
            Qnode *front = node->q.first;
            node->q.first = front->next;
            // the priority is now empty
            if (front->next == NULL)
                node->q.last = NULL;
            delete front;
            return;
        }

        node = node->next;
    }
}

// the num of elements in the pq
int sizePQ(PQ pq) {
    int count = 0;
    while (pq) {
        Qnode *node = pq->q.first;
        while (node) {
            count++;
            node = node->next;
        }

        pq = pq->next;
    }

    return count;
}

// the num of elements in a specific priority
int sizeByPriority(PQ pq, int priority) {
    while (pq && pq->priority <= priority) {
        // this is our priority!
        if (pq->priority == priority) {
            int count = 0;
            Qnode *node = pq->q.first;
            // iterate through it
            while (node) {
                count++;
                node = node->next;
            }

            return count;
        }

        pq = pq->next;
    }

    return 0;
}

// this is my own debugging code - it is never run on the remote server
void test(PQ pq) {
    PQ head = pq;
    cout << "Is empty? " << isEmptyPQ(pq) << endl;
    cout << "Num priorities: " << numPriorities(pq) << endl;
    while (pq) {
        cout << "Priority size: " << sizeByPriority(head, pq->priority) << endl;
        cout << "Priority " << pq->priority <<
            " (" << pq->q.first << "," << pq->q.last << ")" << endl;
        Qnode *node = pq->q.first;
        while (node) {
            cout << "  " << node->val << endl;
            node = node->next;
        }
        pq = pq->next;
        cout << endl;
                    //cin.get();
    }
    cout << endl;
}

// neither is this
int main() {
    PQ pq;
    initPQ(pq);
    
    for (int i = 0; i < 100; i += 5) {
        char buffer[16];
        sprintf(buffer, "%d", i);
        enterPQ(pq, buffer, 10 - (i / 10));

        //if (i % 10 == 9) test(pq);
    }

    //test(pq);

    while (!isEmptyPQ(pq)) {
//        test(pq);
        //cout << endl;
        //cout << firstPQ(pq) << endl;
        leavePQ(pq);
        cout << sizePQ(pq) << endl;
    }

    test(pq);

    return 0;
}