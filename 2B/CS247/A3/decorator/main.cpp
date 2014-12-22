#include <iostream>
#include <cctype>
using namespace std;

#include "Decorator.h"
#include "SandwichDecorator.h"
#include "FunctionDecorator.h"
#include "ColorDecorator.h"

int main() {
    Text output("hello world");
    
    output.decorate(
        new FunctionDecorator<int(*)(int)>(toupper)
    ).decorate(
        new SandwichDecorator("xX-")
    ).decorate(
        new ColorDecorator(color::red)
    ).decorate(
        new SandwichDecorator("(", ")")
    );
    
    cout << output << endl;
    
    return 0;
}