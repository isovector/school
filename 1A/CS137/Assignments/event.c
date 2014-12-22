#include <stdio.h>
#include "event.h"

/*struct tod {
    int hour, minute;
};
  
struct event {
    struct tod start, end;
};*/

int mkctime(struct tod tod) {
    return tod.hour * 60 + tod.minute;
}
  
int freetime (struct event schedule[], int n, int hour, int min) {
    int i = 0, time = hour * 60 + min;

    for (; i < n; i++)
        if (mkctime(schedule[i].start) <= time && mkctime(schedule[i].end) > time)
            return 0;
    return 1;
}


/*int main (void) {
    struct event schedule[] = {{{9,45},{9,55}},{{13,0},
                              {14,20}},{{15,0},{16,30}}};

    printf ("%d\n", freetime (schedule,3,8,0));
    printf ("%d\n", freetime (schedule,3,9,50));
    printf ("%d\n", freetime (schedule,3,13,0));
    printf ("%d\n", freetime (schedule,3,16,30));
}*/
