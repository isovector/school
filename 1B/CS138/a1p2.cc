#include <string>
#include <iostream>
#include <cstdlib>
#include <vector>
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
    char c = get_token()[0];

    if (n < 0) {
        cerr << "Error, line length must be positive." << endl;
        return 1;
    }

    string line;
    char buffer[1024];
    vector<string> lines;

    while (!cin.eof()) {
        cin.getline(buffer, 1024);
        line = buffer;
        if (line.length() != 0)
            lines.push_back(line);
    }

    switch (c) {
        case 'n':
            for (int i = 0; i < lines.size(); i++)
                cout << lines[i].substr(0, n) << endl;
            break;
        case 'r':
            for (int i = lines.size() - 1; i >= 0; i--)
                cout << lines[i].substr(0, n) << endl;
            break;
        case 'f':
            for (int i = 0; i < lines.size(); i++)
                if (lines[i].find("fnord") != -1)
                    cout << lines[i].substr(0, n) << endl;
            break;
        default:
            cerr << "Error, illegal command." << endl;
            return 1;
    }

    return 0;
}