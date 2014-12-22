#include "SandwichDecorator.h"
using namespace std;

SandwichDecorator::SandwichDecorator(string l) : left_(l), right_(l) {
    for (size_t i = 0; i < l.size() / 2; i++)
        swap(right_[i], right_[l.size() - 1 - i]);
}
    
SandwichDecorator::SandwichDecorator(string l, string r) 
    : left_(l), right_(r) 
{ }
    
std::string SandwichDecorator::transform(string input) const {
    return left_ + input + right_;
}