#ifndef DATE_H
#define DATE_H

#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <ctime>
#include <sstream>


class Date {
public:
	Date (int day, std::string month, int year);    // constructor -- NO DEFAULT CONSTRUCTOR
	Date (const Date&);             // copy constructor
	static Date today();            // returns new Date value = today
	~Date();                        // destructor
	int day() const;                // accessor
	std::string month() const;      // accessor
	int year() const;               // accessor
	Date incDays (long) const;      // increment Date by num days - round down if day is invalid, return new Date
	Date incMonths (int) const;     // increment Date by num months - round down if day is invalid, return new Date
	Date incYears (int) const;      // increment Date by num years - round down if day is invalid, return new Date
	Date& operator= (const Date&);  // assignment 
private:
	struct	Impl;
	Impl*	pimpl;
};
	

std::vector<std::string>& getMonths();
std::vector<int>& getDays();
int monthToInt(std::string);
std::string intToMonth(int);
bool isValidDate(int, std::string, int);
bool isLeapYear(int);

bool operator== (const Date&, const Date&);
bool operator!= (const Date&, const Date&);
bool operator< (const Date&, const Date&);
bool operator<= (const Date&, const Date&);
bool operator> (const Date&, const Date&);
bool operator>= (const Date&, const Date&);

std::ostream& operator<< (std::ostream&, const Date&);
std::istream& operator>> (std::istream&, Date&);



#endif