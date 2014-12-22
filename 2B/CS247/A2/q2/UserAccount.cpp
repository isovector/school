#include "UserAccount.h"
#include <iostream>
using namespace std;

UserAccount::UserAccount() {
    userid_.reset(new Userid(cin));
    while (true) {
        try {
            cout << "Enter preferred password: ";
            passwd_.reset(new Password(cin));
            break;
        } catch(std::string error) {
            cout << "Password :\n" << error;
        }
    }
}

void UserAccount::authenticate() {
    if (check_deactivated()) {
        cout << "Account has been deactivated." << endl << endl;
        return;
    }
    
    int tries = 3;
    while (tries --> 0) {
        cout << "Enter password: ";
        try {
            Password password(cin);
            
            if (password == *passwd_)
                return;
        }
        catch (std::string error) {
        }
        
        if (tries)
            cout << "Invalid password. You have " << tries << " more tr" << (tries == 1 ? "y" : "ies") << " to get it right." << endl;
        else {
            cout << "Imposter!! Account is being deactivated!!" << endl;
            deactivate();
        }
    }
    
}

void UserAccount::deactivate() {
    userid_->activate(false);
}

void UserAccount::reactivate() {
    userid_->activate(true);
}

bool UserAccount::check_deactivated() const {
    return userid_->deactivated();
}

