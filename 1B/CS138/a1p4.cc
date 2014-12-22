#include <string>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <deque>
#include <ctype.h>
using namespace std;

typedef deque<string> WORDS;
typedef deque<deque<string> > WORDLINES;

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

int lengthwithspace(int cur, string word) {
    return (cur != 0 ? 1 : 0) + word.size();
}

WORDLINES parse(int len) {
    WORDLINES ret;

    int cur = 0;
    string word = "NOTHINGDEADBEEF";
    while (!cin.eof()) {
    out:
        cur = 0;        
        WORDS words;
        if (word != "NOTHINGDEADBEEF") {
            words.push_back(word);
            cur += word.length();
        }
        while (!cin.eof()) {
            word = get_token().substr(0, len);
            cur += lengthwithspace(cur, word);
            if (cur > len) {
                ret.push_back(words);
                goto out;
            }
            words.push_back(word);
        }
        ret.push_back(words);
    }
    //WORDS line;
    //line.push_back(word);
    //ret.push_back(line);

    return ret;
}

int main(int argc, char *argv[]) {
    int n = atoi(get_token().c_str());

    if (n < 0) {
        cerr << "Error, line length must be positive." << endl;
        return 1;
    }

    WORDLINES lines = parse(n);

    while (lines.size() > 0) {
        int cur = 0;
        WORDS line = lines.front();
        WORDS copy;
        lines.pop_front();
        while (line.size() > 0) {
            string word = line.front();
            copy.push_back(word);
            line.pop_front();
            cur += word.size();
        }

        /*for (int i = 0; i < n; i++) {
            cout << (i + 1) % 10;
        }
        cout << endl;*/

        line = copy;
        int spaces = n - cur;
        int extras = line.size() == 1 ? 0 : spaces % (line.size() - 1);
        //cout << spaces << endl;
        //cout << extras << endl;
        
        int wcount = line.size();
        int count = line.size() == 1 ? 1 : line.size() - 1;
        //cout << count << endl;
        while (line.size() > 0) {
            string word = line.front();
            line.pop_front();
            cout << word;
            int thisextra = (extras-- > 0) ? 1 : 0;
            if (line.size() != 0 || wcount == 1)
                for (int i = 0; i < spaces / count + thisextra; i++)
                    cout << ' ';
        }

        cout /*<< "|"*/ << endl;
    }

    return 0;
}