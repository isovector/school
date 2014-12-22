#ifndef __DECORATOR_H
#define __DECORATOR_H

#include <iostream>
#include <string>

class Decorator {
    Decorator *decorator_;

public:
    Decorator();
    
    virtual ~Decorator();
    std::string operator()(std::string input) const;
    Decorator &decorate(Decorator *decorator);
    virtual std::string transform(std::string) const = 0;
};


class Text : public Decorator {
    friend std::ostream &operator <<(std::ostream &out, Text&);
    std::string text_;
    
public:
    Text(std::string t);
    std::string str() const;
    virtual std::string transform(std::string input) const;
};

std::ostream &operator <<(std::ostream &out, Text &t);

#endif
