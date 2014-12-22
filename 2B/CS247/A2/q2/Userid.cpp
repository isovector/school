#include "Userid.h"
using namespace std;

set<string> Userid::used_ids;

Userid::Userid(std::istream &stream) : activated_(true) {
    bool error;
    do {
        error = false;
        cout << "Enter preferred userid: ";
        stream >> name_;
        
        if (name_.find_first_of(" \t\v") != std::string::npos) {
            cout << "Userid may not contain spaces." << endl; 
            error = true;
        }
        if (used_ids.count(name_)) {
            cout << "Userid \"" << name_ << "\" is already taken." << endl;
            error = true;
        }
    } while(error);
    
    used_ids.insert(name_);
}

Userid::~Userid() {
    used_ids.erase(name_);
}

void Userid::activate(bool enable) {
    activated_ = enable;
}

bool Userid::deactivated() const {
    return !activated_;
}
