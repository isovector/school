#include "q2table.h"

// The one cc file to rule them all:
// includes the correct table file depending on which
// TABLETYPE is supplied. This significantly simplifies
// the logic of what otherwise would have been here.

#if defined( TABLETYPE_INT )
    #include "q2table_int.cc"
#elif defined( TABLETYPE_INTB )
    #include "q2table_intb.cc"
#elif defined( TABLETYPE_AUTO )
    #include "q2table_auto.cc"
#elif defined( TABLETYPE_TASK )
    #include "q2table_task.cc"
#else
    #error unsupported table
#endif

// All tables share the same constructor
Table::Table(unsigned int NoOfPhil, Printer &prt) :
    numPhil(NoOfPhil), printer(prt)
{ 
    for (size_t i = 0; i < numPhil; ++i) {
        // Tables can conditionally define a TABLE_PHIL_LOOP
        // to initialize other things for each philosopher
        #ifdef TABLE_PHIL_LOOP
        TABLE_PHIL_LOOP(i)
        #endif
        
        forksAvailable.push_back(true);
    }
}
