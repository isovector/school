#ifndef Q2_TABLE_H
#define Q2_TABLE_H

#include "MPRNG.h"
#include "q2printer.h"

#include <uSemaphore.h>

// The one header to rule them all:
// conditionally include the correct table header

#if defined( TABLETYPE_INT )
    #include "q2table_int.h"
#elif defined( TABLETYPE_INTB )
    #include "q2table_intb.h"
#elif defined( TABLETYPE_AUTO )
    #include "q2table_auto.h"
#elif defined( TABLETYPE_TASK )
    #include "q2table_task.h"
#else
    #error unsupported table
#endif

#endif
