#include <string>
#include <cctype>
using namespace std;

int scrabbleValue(char c) {
    c = tolower(c);
    switch (c) {
        case 'e':
        case 'a':
        case 'i':
        case 'o':
        case 'n':
        case 'r':
        case 't':
        case 'l':
        case 's':
        case 'u':
            return 1;
        case 'd':
        case 'g':
            return 2;
        case 'b':
        case 'c':
        case 'm':
        case 'p':
            return 3;
        case 'f':
        case 'h':
        case 'v':
        case 'w':
        case 'y':
            return 4;
        case 'k':
            return 5;
        case 'j':
        case 'x':
            return 8;
        case 'q':
        case 'z':
            return 10;
    }

    return 0;
}

int scrabbleValue(string word) {
    int count = 0;
    for (int i = 0; i < word.length(); i++)
        count += scrabbleValue(word[i]);
    return count;
}

int main() {}