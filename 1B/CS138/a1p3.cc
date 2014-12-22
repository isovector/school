#include <string>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <deque>
#include <ctype.h>
using namespace std;

string get_token() {
    string s;
    cin >> s;
    
    char c;
    do
        c = cin.get();
    while(isspace(c));
    cin.putback(c);

    return s;
}

int main(int argc, char *argv[]) {
    int n = atoi(get_token().c_str());

    if (n < 0) {
        cerr << "Error, line length must be positive." << endl;
        return 1;
    }

    deque<string> words;

    while (!cin.eof()) {
        words.push_back(get_token().substr(0, n));
    }

    while (words.size() > 0) {
        string line = "";
        do { 
            string word = words.front();
            words.pop_front();
            if (line.length() + word.length() + (line.length() != 0 ? 1 : 0) > n) {
                words.push_front(word);
                break;
            }
            line += (line.length() != 0 ? " " : "") + word;
        } while(line.length() < n && words.size() > 0);
        cout << line << endl;
    }

    return 0;
}