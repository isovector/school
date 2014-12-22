#include <iostream>
#include <memory>
#include "UserAccount.h"

using namespace std;


//************************************************************************
//  Test Harness Helper functions
//************************************************************************

//  test-harness operators
enum Op { NONE, New, Delete, Authenticate, Deactivate, Reactivate, CheckDeactivated };

Op convertOp( string opStr ) {
    switch( opStr[0] ) {
        case 'n': return New; 
        case 'D': return Delete;
        case 'a': return Authenticate;
        case 'd': return Deactivate;
        case 'r': return Reactivate;
        case 'c': return CheckDeactivated;
        default: {
            cout << "Invalid command " << opStr << endl;
            return NONE;
        }
    }
}


// Read account handle (1 or 2) from cin 
int getAccount() {
    int num;
    cin >> num;
    if ( num != 1 && num != 2 ) {
        cout << "Invalid account handle!" << endl;
        return 0;
    }
    
    if ( cin.fail() ) {
        cin.clear();
        string discard;
        getline(cin, discard);
    }
    
    return num;
}




//******************************************************************
// Test Harness for UserAccount // Userid // Password ADTs
//******************************************************************

int main() {
    cout << "Test harness for UserAccount ADT:" << endl << endl;
    auto_ptr<UserAccount> x1;
    auto_ptr<UserAccount> x2;

    
    cout << "Command: ";
    string command;
    cin >> command;
    
    Op op = convertOp( command );

    while ( !cin.eof() ) {		
    switch ( op ) {
                
        // Construct new user account
        case New: {
            int ap = getAccount();
            if ( ap == 1 )
                x1.reset( new UserAccount() );
            else if ( ap == 2 )
                x2.reset( new UserAccount() );
            break;
        }
                
        case Delete: {
            int ap = getAccount();
            if ( ap == 1 ) 
                x1.reset();
            else if ( ap == 2 )
                x2.reset();
            break;
        }
                
			
        // Test authentication routine
        case Authenticate: {
            int ap = getAccount();
            if ( ap == 1 ) {
                if ( x1.get() )
                    x1->authenticate();
                else
                    cout << "x1 is unbound." << endl;
            }
            else {
                if ( ap == 2 ) {
                    if ( x2.get() )
                        x2->authenticate();
                    else
                        cout << "x2 is unbound." << endl;
                }
            }
            break;
        }
                    

        // Deactivate user account
        case Deactivate: {
            int ap = getAccount();
            if ( ap == 1 ) {
                if ( x1.get() )
                    x1->deactivate();
                else
                    cout << "x1 is unbound." << endl;
            }
            else if ( ap == 2 ) {
                if ( x2.get() )
                    x2->deactivate();
                else
                    cout << "x2 is unbound." << endl;
            }

            break;
        }


        // Reactivate user account
        case Reactivate: {
            int ap = getAccount();
            if ( ap == 1 ) {
                if ( x1.get() )
                    x1->reactivate();
                else
                    cout << "x1 is unbound." << endl;
            }
            if ( ap == 2 ) {
                if ( x2.get() )
                    x2->reactivate();
                else
                    cout << "x2 is unbound." << endl;
            }
            break;
        }

                
        // Check if user account is deactivated
        case CheckDeactivated: {
            int ap = getAccount();
            if ( ap == 1 ) {
                if ( x1.get() ) {
                    if ( x1->check_deactivated() ) 
                        cout << "x1's account has been deactivated." << endl;
                    else
                        cout << "x1's account has not been deactivated." << endl;
                }
                else 
                    cout << "x1 is unbound." << endl;
            }
            if ( ap == 2 ) {
                if ( x2.get() ) {
                    if ( x2->check_deactivated() ) 
                        cout << "x2's account has been deactivated." << endl;
                    else
                        cout << "x2's account has not been deactivated." << endl;
                }
                else 
                    cout << "x2 is unbound." << endl;
            }
            break;
        }
                
        default:
            break;
        } // switch
						
        cout << endl << "Command: ";
        cin >> command;
        op = convertOp(command);
		
    } // while cin OK
    
    return 0;
}
