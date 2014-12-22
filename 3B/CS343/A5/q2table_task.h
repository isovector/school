#include <deque>

_Task Table {
    void main();
    void setWaiting(size_t id);
    void relinquishForkMonopoly();

    // ugly gross structure, see setWaiting() definition in q2table_task.cc for
    // explanation of WTF is going on with this queue
    std::deque<int> hungryPhils;
    std::vector<uCondition*> forkLocks;
    
    size_t requestedId;

    // include the common mixins
    #include "q2table_common.h"
    
  public:
    virtual ~Table();
};
