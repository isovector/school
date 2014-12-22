#ifndef A3_Q1MAPPER_H
#define A3_Q1MAPPER_H

#include <uSemaphore.h>
#include <string>
#include <iostream>
#include <fstream>

#include "q1kvqueue.h"

_Task Mapper {
    void main();
    
  public:
    // This makes our dependency graph a lot simpler without
    // changing the public interface =)
    typedef KVQueue::KeyValue KeyValue;
    
    Mapper(const std::string& filename, int queue_len, uSemaphore* signal);
    virtual ~Mapper();
    uSemaphore* getSignal();
    const std::string& getFilename();
    KVQueue* q;
  
  protected:
    uSemaphore *signal;
    std::string filename;
    std::ifstream stream;
};

#endif
