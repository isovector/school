#include <vector>

_Monitor Table {
    // lock to block if we are unable to get both forks
    uCondition tableLock;
    
    // include the common mixins
    #include "q2table_common.h"
};
