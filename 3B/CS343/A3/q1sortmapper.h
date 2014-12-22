#ifndef A3_Q1SORTMAPPER_H
#define A3_Q1SORTMAPPER_H

#include "q1mapper.h"

_Task SortMapper : public Mapper {
    void main();
  public:
    SortMapper(const std::string& filename, int queue_len, int buffer_size, uSemaphore* signal);
    virtual ~SortMapper();
  
  private:
    // ints are NOT supposed to be used for buffer sizes.
    size_t buffer_size;
};

#endif
