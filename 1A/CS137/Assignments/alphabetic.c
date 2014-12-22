#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *alphabetic (const char *s) {
    char *output, *first;

    if (s == 0) return 0;
    
    output = (char*)calloc(strlen(s), sizeof(char));
    first = output;

    while (*s) {
        if (isalpha(*s)) {
            *output = *s;
            ++output;
        }
        ++s;
    }
    *output = '\0';

    return first;
}

/*int main (void) {
  char *a, *b, *c;
  a = alphabetic ("Ready... aim... fire!");
  b = alphabetic ("***");
  c = alphabetic ("*a*b*c*");
  
  printf ("%s\n", a);
  printf ("%s\n", b);
  printf ("%s\n", c);

  free(a);
  free(b);
  free(c);
  
  return 0;
}*/
