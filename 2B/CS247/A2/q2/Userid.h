#ifndef A2_USERID_H
#define A2_USERID_H

#include <string>
#include <set>
#include <iostream>

class Userid {
public:
    explicit Userid(std::istream&);
    ~Userid();
    void activate(bool enable);
    bool deactivated() const;

private:
    static std::set<std::string> used_ids;
    std::string name_;
    bool activated_;
};

#endif