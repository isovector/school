#ifndef __FUNCTION_DECORATOR_H
#define __FUNCTION_DECORATOR_H

#include <algorithm>

#include "Decorator.h"

template<typename P>
class FunctionDecorator : public Decorator {
    P function_;
    
public:
    FunctionDecorator(P func) 
        : function_(func)
    { }

    virtual string transform(string input) const {
        std::string output;
        output.resize(input.size());
        std::transform(input.begin(), input.end(), output.begin(), function_);
        
        return output;
    }
};

#endif
