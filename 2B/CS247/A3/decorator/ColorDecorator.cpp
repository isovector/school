#include <sstream>
#include "ColorDecorator.h"

using namespace std;


ColorDecorator::ColorDecorator(color::color col) {
    string type = "22";
    int icol = col;
    
    if (icol > 8) {
        type = "01";
        icol -= 8;
    }
    
    stringstream str;
    str << "\e[" << type << ';' << 30 + icol << 'm';
    escape_ = str.str();
}

string ColorDecorator::transform(string input) const {
    return escape_ + input + "\e[0m";
}