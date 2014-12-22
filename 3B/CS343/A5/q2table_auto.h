// Creates necessary structures in class definitions
#define AUTOMATIC_SIGNAL \
    uCondition __automatic_signal_condition;
    
// Blocks the thread until a predicate is true
#define WAITUNTIL(pred, before, after) \
    while(!pred){ \
        before; \
        __automatic_signal_condition.wait(); \
        after; \
    }

// Signals all other blocked threads to try again
#define RETURN(expr) \
    while(!__automatic_signal_condition.empty()){ \
        __automatic_signal_condition.signal(); \
    } \
    return expr;


_Monitor Table {
    AUTOMATIC_SIGNAL;

    // include the common mixins
    #include "q2table_common.h"
};
