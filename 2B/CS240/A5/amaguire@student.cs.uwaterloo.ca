#include <string>
#include <map>
#include <iostream>
#include <fstream>
using namespace std;

#include "bitbuffer.h"

#define BITS 10
#define OFFSET 256

BitBuffer bits;
map<string, int> dict;
int next = 0;
bool shouldrem = false;


void push(int c) {
    int mask = 1 << 9;
    for (int i = 0; i < BITS; i++) {
        bits.addBit((c & mask) != 0);
        cout << ((c & mask) != 0);
        mask >>= 1;
    }
    cout << ":";
}

char chomp(string &s) {
    char c = s[0];
    s = s.substr(1);
    return c;
}

string best_prefix(string input) {
    string best = string() + chomp(input);

    while (dict.count(best + input[0]))
        best += chomp(input);
    
    return best;
}

int main(int argc, char **argv) {
    ifstream input(argv[1]);
    ofstream output(argv[2]);
    
    string s = "";
    while (input.peek() != -1 && input)
        s += input.get();
    
    for (int i = 0; i < 256; i++)
        dict[string() + char(i)] = i;
    
    while (s.size()) {
        string w = best_prefix(s);
        push(dict[w]);
        
        if (++next + OFFSET >= 1024) {
            shouldrem = true;
            next = 1;
        }
        
        if (shouldrem)
            for (map<string, int>::iterator it = dict.begin(); it != dict.end(); it++)
                if ((*it).second == OFFSET + next) {
                    dict.erase(it);
                    break;
                }
        
        dict[w + s[w.size()]] = OFFSET + next;
        cout << w << endl;
        s = s.substr(w.size());
    }
    
    push(OFFSET);
    for (int i = 0; i < 7; i++)
        bits.addBit(0);
    
    while (bits.byteReady())
        output.put(bits.getByte());
    output.close();
    
    return 0;
}