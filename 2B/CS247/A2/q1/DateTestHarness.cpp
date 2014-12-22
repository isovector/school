#include <string>
#include <iostream>
#include "Date.h"

using namespace std;

//************************************************************************
//  Helper variables and functions for test harness
//************************************************************************

//  operators
enum Op {NONE, Con, Copy, Assign, Read, Print, Acc, Inc, Today, EQ, NEQ, GT, GTE, LT, LTE};


Op convertOp(string opStr) {
    switch (opStr[0]) {
        case 'c': return Con; 
        case 'C': return Copy;
        case 'A': return Assign;
        case 'r': return Read;
        case 'p': return Print;
        case 'a': return Acc;
        case 'i': return Inc; 
        case 't': return Today;
        case '=': return EQ; 
        case '!': return NEQ; 
        case '<': {
            if ( opStr[1] == '=' ) {
                return LTE;
            }
            else {
                return LT;
            }
        }
        case '>': {
            if ( opStr[1] == '=' ) {
                return GTE;
            }
            else {
                return GT;
            }
        }
        default: return NONE;
    }
}


bool readName(int& index) {	
    char d;
    cin >> d;
    if ( d != 'd' ) {
        cout << "Invalid name of date variable." << endl;
        return false;
    }
	
    index = -1;
    cin >> index;
    if ( index < 0 || index > 9 ) {
        cout << "Invalid name of date variable." << endl;
        return false;
    }
    else {
        return true;
    }
}


void fixcin () {
    cin.clear();
    string discard;
    getline(cin, discard);
}

//*******************
// main()
//*******************

int main () {
    cout << "Test harness for Date ADT" << endl << endl;
    Date *dates[10] = {0};
	
    cout << "Command: ";
    string command;
    cin >> command;
	
    Op op = convertOp(command);
	
    while ( !cin.eof() ) {
        switch (op) {
            //  constructor
            case Con: {
                try {
                    int index;
                    if ( readName(index) ) {
                        int day;
                        cout << "Enter day of month (1-31): ";
                        cin >> day;
                        if ( cin.fail() ) {
                            fixcin();
                            cout << "Day must be an integer." << endl;
                            break;
                        }
					
                        string month;
                        cout << "Enter month (January-December): ";
                        cin >> month;
					
                        int year;
                        cout << "Enter year (1900-2100): ";
                        cin >> year;
                        if ( cin.fail() ) {
                            fixcin();
                            cout << "Year must be an integer." << endl;
                            break;
                        }		
    
                        dates[index] = new Date(day, month, year);
                    }
                }
                catch ( const char* &s ) { 
                    cout << s << endl;
                }
                break;
            }
				
            // accessors
            case Acc: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] != 0) {
                        cout << "day = " << dates[index]->day() << endl;
                        cout << "month = " << dates[index]->month() << endl;
                        cout << "year = " << dates[index]->year() << endl;
                    }
                    else
                        cout << "Date d" << index << " is not initialized." << endl;
                }
                break;
            }
				
				
            // today()
            case Today: {
                cout << "Today is " << Date::today() << endl;
                break;
            }
				
				
            // Inc
            case Inc: {
                try {
                    int index;
                    if ( readName(index) ) {
                        if (dates[index] != 0) {
                            int inc;
                            cout << "Enter number of years to increment:";
                            cin >> inc;
                            if (cin.fail() || inc<0) {
                                fixcin();
                                cout << "Invalid increment." << endl;
                                break;
                            }
                            Date d1 = dates[index]->incYears(inc);
                            cout << *dates[index] << " + " << inc << " years = " << d1 << endl;
						
                            cout << "Enter number of months to increment:";
                            cin >> inc;
                            if (cin.fail() || inc<0) {
                                fixcin();
                                cout << "Invalid increment." << endl;
                                break;
                            }
                            Date d2 = dates[index]->incMonths(inc);
                            cout << *dates[index] << " + " << inc << " months = " << d2 << endl;
						
                            cout << "Enter number of days to increment:";
                            cin >> inc;
                            if (cin.fail() || inc<0) {
                                fixcin();
                                cout << "Invalid increment." << endl;
                                break;
                            }
                            Date d3 = dates[index]->incDays(inc);
                            cout << *dates[index] << " + " << inc << " days = " << d3 << endl;
                        }
                        else {
                            cout << "Date d" << index << " is not initialized." << endl;
                        }
                    }
                }
                catch ( const char* &s ) {
                    cout << s << endl;
                }
                break;
            }
						
						
            // operator>>
            case Read: {
                try{
                    int index;
                    if ( readName(index) ) {
                        if (dates[index] != 0) {
                            cout << "Enter date value (day month, year): ";
                            cin >> *dates[index];
                            if ( !cin.fail() )
                                cout << "Date d" << index << " = " << *dates[index] << endl;
                            else {
                                fixcin();
                                cout << "Invalid date value." << endl;
                            }
                        }
                        else
                            cout << "Date d" << index << " is not initialized." << endl;
                    }
                }
                catch ( const char* &s ) { 
                    cout << s << endl;
                }                
                break;
            }
				
            // operator<<
            case Print: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] != 0) 
                        cout << "Date d" << index << " = " << *dates[index] << endl;
                    else
                        cout << "Date d" << index << " is not initialized." << endl;
                }
                break;				
            }
				
            case EQ: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        fixcin();
                        cout << "Date d" << index << " is not initialized." << endl;
                        break;
                    }
                    int index2;
                    if ( readName(index2) ) {
                        if (dates[index2] == 0) {
                            fixcin();
                            cout << "Date d" << index2 << " is not initialized." << endl;
                            break;
                        }
                        cout << *dates[index] << " == " << *dates[index2] << " is " << ( (*dates[index]==*dates[index2]) ? "true" : "false" ) << endl;
                    }
                }
                break;
            }
				
            case NEQ: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        fixcin();
                        cout << "Date d" << index << " is not initialized." << endl;
                        break;
                    }
                    int index2;
                    if ( readName(index2) ) {
                        if (dates[index2] == 0) {
                            fixcin();
                            cout << "Date d" << index2 << " is not initialized." << endl;
                            break;
                        }
                        cout << *dates[index] << " != " << *dates[index2] << " is " << ( (*dates[index]!=*dates[index2]) ? "true" : "false" ) << endl;
                    }
                }
                break;
            }

				
            case LT: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        fixcin();
                        cout << "Date d" << index << " is not initialized." << endl;
                        break;
                    }
                    int index2;
                    if ( readName(index2) ) {
                        if (dates[index2] == 0) {
                            fixcin();
                            cout << "Date d" << index2 << " is not initialized." << endl;
                            break;
                        }
                        cout << *dates[index] << " < " << *dates[index2] << " is " << ( (*dates[index]<*dates[index2]) ? "true" : "false" ) << endl;
                    }
                }
                break;
            }

				
            case LTE: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        fixcin();
                        cout << "Date d" << index << " is not initialized." << endl;
                        break;
                    }
                    int index2;
                    if ( readName(index2) ) {
                        if (dates[index2] == 0) {
                            fixcin();
                            cout << "Date d" << index2 << " is not initialized." << endl;
                            break;
                        }
                        cout << *dates[index] << " <= " << *dates[index2] << " is " << ( (*dates[index]<=*dates[index2]) ? "true" : "false" ) << endl;
                    }
                }
                break;
            }

				
            case GT: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        cout << "Date d" << index << " is not initialized." << endl;
                        fixcin();
                        break;
                    }
                    int index2;
                    if ( readName(index2) ) {
                        if (dates[index2] == 0) {
                            cout << "Date d" << index2 << " is not initialized." << endl;
                            fixcin();
                            break;
                        }
                        cout << *dates[index] << " > " << *dates[index2] << " is " << ( (*dates[index]>*dates[index2]) ? "true" : "false" ) << endl;
                    }
                }
                break;
            }

				
            case GTE: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        fixcin();
                        cout << "Date d" << index << " is not initialized." << endl;
                        break;
                    }
                    int index2;
                    if ( readName(index2) ) {
                        if (dates[index2] == 0) {
                            fixcin();
                            cout << "Date d" << index2 << " is not initialized." << endl;
                            break;
                        }
                        cout << *dates[index] << " >= " << *dates[index2] << " is " << ( (*dates[index]>=*dates[index2]) ? "true" : "false" ) << endl;
                    }
                }
                break;
            }
				
				
            case Copy: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        fixcin();
                        cout << "Date d" << index << " is not initialized." << endl;
                        break;
                    }
                    Date d(*dates[index]);
                    cout << "New date = " << d << ", old date = " << *dates[index] << endl;
                }
                break;
            }
				
				
            case Assign: {
                int index;
                if ( readName(index) ) {
                    if (dates[index] == 0) {
                        fixcin();
                        cout << "Date d" << index << " is not initialized." << endl;
                        break;
                    }

                    int index2;
                    if ( readName(index2) ) {
                        if (dates[index2] == 0) {
                            fixcin();
                            cout << "Date d" << index2 << " is not initialized." << endl;
                            break;
                        }
						
                        *dates[index] = *dates[index2];
                        cout << "New date = " << *dates[index] << ", old date = " << *dates[index2] << endl;
                    }
                }
                break;
            }
				
            default: {
                cout << "Invalid command." << endl;
                break;
            }
        } // switch command
		
        cout << endl << "Command: ";
        cin >> command;
        op = convertOp(command);
		
    } // while cin OK
    
    for ( int i=0; i<10; i++ ) {
        delete dates[i];
    }
}
		