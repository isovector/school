#include "q1kvqueue.h"
using namespace std;

// Introduces a critical sction until the end of the scope.
#define CRITICAL_SECTION \
    RAII<uSemaphore> __scoped_lock(lock, &uSemaphore::P, &uSemaphore::V);
// I know, I know. Macros are evil, but IMO this is much more
// readable than its expansion.

KVQueue::KVQueue(int size) :
    qFull(size),
    qEmpty(0),
    closed(false),
    lock(1)
{
}

void KVQueue::pushBack(const KeyValue& item) {
    // Pause if the queue is full
    qFull.P();
    
    CRITICAL_SECTION
    queue.push_back(item);
    qEmpty.V();
}

KVQueue::KeyValue KVQueue::popFront() {
    // Pause if the queue is empty
    qEmpty.P();

    CRITICAL_SECTION
    KeyValue kv = queue.front();
    queue.pop_front();
    qFull.V();
    
    return kv;
}

// The assignment is unclear about what this should return
// so I've decided to make it the number of queued elements.
int KVQueue::peekFront(KeyValue* val) throw(EmptyAndClosed) {
    CRITICAL_SECTION
    
    if (queue.empty()) {
        if (closed) {
            // This argument is never used, and thus is not
            // implemented.
            throw EmptyAndClosed(0);
        }
        
        return 0;
    }
    
    *val = queue.front();
    return queue.size();
}

void KVQueue::close() {
    CRITICAL_SECTION
    
    closed = true;
}

// Let's just pretend like this whole thing never happened.
#undef CRITICAL_SECTION
