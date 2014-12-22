#ifndef A3_Q1REDUCER_H
#define A3_Q1REDUCER_H

#include "uSemaphore.h"
#include <vector>
#include <string>

#include "q1mapper.h"

_Task Reducer {
    void main();
  public:
    Reducer(int id, int num_reducers, uSemaphore* signal, const std::vector<Mapper*>& mappers);
    virtual ~Reducer();

    unsigned long hash(const std::string& str);
    int getID();
    int getNumReducers();
    uSemaphore* getSignal();
    std::vector<Mapper*>& getMappers();
  
  protected:
    // Whoever wrote these assignments seems woefully unaware of the size_t type
    size_t id;
    size_t num_reducers;
    uSemaphore *signal;
    std::vector<Mapper*> mappers;
};

#endif
