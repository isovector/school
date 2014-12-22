#include "Userid.h"
#include "Password.h"
#include <memory>

class UserAccount {
public:
    UserAccount();  
    // constructs new account, querying the user for the userid and password
    // user is repeated prompted for userid and password until values are valid

    void authenticate();                
    // asks user for password; deactivates account after 3 incorrect guesses

    void deactivate();                  
    // deactivates account: userid still exists, but cannot authenticate
 
    void reactivate();                  
    // reactivates the account, asking the user for a new valid password
    // user is repeatedly prompted for a password until a valid value is provided

    bool check_deactivated() const;     
    // returns true if account is deactivated; otherwise, returns false
    
private:
    std::auto_ptr<Userid> userid_;
    std::auto_ptr<Password> passwd_;
};