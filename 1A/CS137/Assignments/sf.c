#include <stdio.h>
#include <assert.h>
#include "sf.h"

/*struct card {
    int value;
    char suit;
};*/

int suitToIndex(char c) {
    switch (c) {
        case 'c': return 0;
        case 'd': return 1;
        case 'h': return 2;
        case 's': return 3;
        default: assert(0);
    }
} 

int straightflush(struct card hand[], int n) {
    int cards[4][14] = {0}, i = 0, j = 0, sequential = 0;
    char suit;

    for (; i < n; i++) {
        suit = suitToIndex(hand[i].suit);
        cards[suit][hand[i].value - 1] = 1;
        if (hand[i].value == 1)
            cards[suit][13] = 1;
    }

    for (; j < 4; j++) {
        sequential = 0;
        for (i = 0; i < 14; i++) {
            sequential += cards[j][i] ? 1 : -sequential;
            if (sequential >= 5)
                return 1;
        }
    }

    return 0;
}

/*int main() {
    struct card hand1[] = {{4,'s'}, {9,'s'},{12,'c'},{11,'s'},{8,'s'},
                           {6,'d'}, {3,'d'},{7,'s'},{10,'s'},{12,'d'}};
    struct card hand2[] = {{8,'c'}, {2,'h'},{5,'s'},{6,'c'},{1,'s'},
                           {5,'c'}, {4,'d'},{6,'h'},{13,'d'},{1,'d'}};
    
    printf ("%d\n", straightflush(hand1, 10));
    printf ("%d\n", straightflush(hand2, 10));
	
    return 0;
}
//*/
