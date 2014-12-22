#ifndef __SANDWICH_DECORATOR_H
#define __SANDWICH_DECORATOR_H

#include "Decorator.h"

class SandwichDecorator : public Decorator {
    std::string left_;
    std::string right_;
    
public:
    SandwichDecorator(std::string l);
    SandwichDecorator(std::string l, std::string r);
    virtual std::string transform(std::string input) const;
};

#endif
