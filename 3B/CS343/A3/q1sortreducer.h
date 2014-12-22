#ifndef A3_Q1SORTREDUCER_H
#define A3_Q1SORTREDUCER_H

#include "q1reducer.h"

_Task SortReducer : public Reducer {
    void main();
  public:
    SortReducer(int id, int num_reducers, uSemaphore* signal, const std::vector<Mapper*>& mappers);
    virtual ~SortReducer();
};

#endif
