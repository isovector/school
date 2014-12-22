#include "Decorator.h"
using namespace std;

Decorator::Decorator()
    : decorator_(NULL) 
{ }

Decorator::~Decorator() {
    if (decorator_)
        delete decorator_;
}

string Decorator::operator()(string input) const {
    input = transform(input);
    if (decorator_)
        input = decorator_->operator()(input);
    return input;
}

Decorator &Decorator::decorate(Decorator *decorator) {
    if (decorator_)
        decorator_->decorate(decorator);
    else
        decorator_ = decorator;
    return *this;
}


Text::Text(string t) 
    : Decorator(), text_(t) 
{ }

string Text::str() const {
    return Decorator::operator()(text_);
}

string Text::transform(string input) const {
    return input;
}

ostream &operator <<(ostream &out, Text &t) {
    return out << t.str();
}
