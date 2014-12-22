#ifndef A2_PASSWORD_H
#define A2_PASSWORD_H

#include <iostream>

class Password {
public:
    explicit Password(std::istream&);
    Password(const Password&);
    bool operator==(const Password&);
    bool operator!=(const Password&);

private:
    std::string password_;
};

#endif
