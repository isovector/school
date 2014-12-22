#include <string>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <ctype.h>
using namespace std;

string get_token() {
    string s;
    cin >> s;
    
    /*char c;
    do
        c = cin.get();
    while(isspace(c));
    cin.putback(c);*/

    return s;
}

int main(int argc, char *argv[]) {
    int n = atoi(get_token().c_str());

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

    for (int i = 0; i < lines.size(); i++)
        cout << lines[i].substr(0, n) << endl;

    return 0;
}