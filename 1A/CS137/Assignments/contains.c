#include <stdio.h>
#include <string.h>

int contains(char *s, char *t) {
    int counter = 0, success = 1;
    char *p = t, *q;

    if (s == (char*)0 || t == (char*)0 || strlen(t) > strlen(s)) 
        return 0;

    while (*s != '\0') {
        if (*s == *t) {
            q = s + 1;
            success = 1;
            while (*t != '\0') {
                if (*s != *t) {
                    success = 0;
                    break;
                }
                ++s;
                ++t;
            }
            if (success == 1)
                counter++;
            t = p;
            s = q;
        } else ++s;
    }
    return counter;
}

/*int main(void) {
  printf ("%d\n", contains ("papapapa", "papa"));

  return 0;
}*/

