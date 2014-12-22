#ifndef A3_Q1KVQUEUE_H
#define A3_Q1KVQUEUE_H

#include <uSemaphore.h>
#include <deque>
#include <iostream>

// A template class to wrap RAII semantics around objects which do not
// implement them natively. Used by KVQueue to implement critical sections.
template<typename T>
class RAII {
  public:
    // Func is a method on type T that takes and returns nothing
    typedef void (T::*Func)();
  
    RAII(T &val, Func acquire, Func release) : 
        val(val), 
        release(release) 
    {
        // Call the acquire method on the wrapped value
        (val.*acquire)();
    }
    
    ~RAII() { 
        (val.*release)(); 
    }
    
  private:
    T &val;
    Func release;
};

class KVQueue {    
  public:
    struct KeyValue {
        std::string key;
        int value;

        KeyValue(const std::string& key, int value) : key(key), value(value) {}
        KeyValue() {}
    };
  
    struct EmptyAndClosed {
        int num_thrown;
        EmptyAndClosed(int num_thrown) : num_thrown(num_thrown) {}
    };
    
    KVQueue(int size);
    void pushBack(const KeyValue& item);
    KeyValue popFront();
    int peekFront(KeyValue* val) throw(EmptyAndClosed);
    void close();
    
  private:
    // qFull blocks if the queue is full, qEmpty if empty
    uSemaphore qFull, qEmpty;
    bool closed;
    std::deque<KeyValue> queue;
  
    // The internal lock which is scope-wrapped by the RAII container above
    uSemaphore lock;
};

#endif
