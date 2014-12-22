#ifndef __COLOR_DECORATOR_H
#define __COLOR_DECORATOR_H

#include "Decorator.h"

namespace color {
    enum color {
        black,
        red,
        green,
        brown,
        blue,
        magenta,
        cyan,
        gray,
        dark_gray,
        light_red,
        light_green,
        yellow,
        light_blue,
        light_magenta,
        light_cyan,
        white
    };
}

class ColorDecorator : public Decorator {
    std::string escape_;
    
public:
    ColorDecorator(color::color);
    virtual std::string transform(std::string) const;
};

#endif
