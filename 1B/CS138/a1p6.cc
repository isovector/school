#include <string>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <deque>
#include <fstream>
#include <ctype.h>
using namespace std;

typedef deque<string> WORDS;
typedef deque<deque<string> > WORDLINES;

string get_token(istream &stream) {
    string s;
    stream >> s;
    
    char c;
    do
        c = stream.get();
    while(isspace(c));
    stream.putback(c);

    return s;
}

int lengthwithspace(int cur, string word) {
    return (cur != 0 ? 1 : 0) + word.size();
}

WORDLINES parse(istream &stream, int len) {
    WORDLINES ret;

    int cur = 0;
    string word = "NOTHINGDEADBEEF";
    while (!stream.eof()) {
    out:
        cur = 0;        
        WORDS words;
        if (word != "NOTHINGDEADBEEF") {
            words.push_back(word);
            cur += word.length();
        }
        while (!stream.eof()) {
            word = get_token(stream).substr(0, len);
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

void printit(WORDLINES lines, int n, string command, string extra) {
    int which = 1;
    while (lines.size() > 0) {
        bool fnord = false;
        int cur = 0;
        WORDS line;

        if (command[0] == 'r') {
            line = lines.back();
            lines.pop_back();
        } else {
            line = lines.front();
            lines.pop_front();
        }

        WORDS copy;
        while (line.size() > 0) {
            string word = line.front();
            if (command == "f" && word.find(extra) != -1) fnord = true;
            copy.push_back(word);
            line.pop_front();
            cur += word.size();
        }

        if (command[0] == 'f' && !fnord)
            continue;

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
        if ((command[0] == 'p' && which == atoi(extra.c_str())) || command[0] != 'p') {
            while (line.size() > 0) {
                string word = line.front();
                line.pop_front();
                cout << word;
                int thisextra = (extras-- > 0) ? 1 : 0;
                int totalspaces = (command[command.length() - 1] == 'j' ? spaces / count + thisextra : 1);
                if (line.size() != 0 || (wcount == 1 && command[command.length() - 1] == 'j'))
                    for (int i = 0; i < totalspaces; i++)
                        cout << ' ';
            }

            cout <<  endl;
        }

        which++;
    }
}

int main(int argc, char *argv[]) {
    int n = atoi(get_token(cin).c_str());
    ifstream file(get_token(cin).c_str());
    
    WORDLINES lines;

    if (n < 0) {
        cerr << "Error, line length must be positive." << endl;
        return 1;
    }

    if (!file) {
        cerr << "Error, can't open specified text file." << endl;
        return 1;
    }

    lines = parse(file, n);

    while (!cin.eof()) {
        string command = get_token(cin);
        string extra = "";
        
        if (command == "n");
        else if (command == "r");
        else if (command == "j");
        else if (command == "rj");
        else if (command == "f"
            | command == "p"
            | command == "pj")
            extra = get_token(cin);
        else if (command == "q")
            return 0;
        else {
            cerr << "Error, illegal command." << endl;
            return 1;
        }

        printit(lines, n, command, extra);
    }

    return 0;
}