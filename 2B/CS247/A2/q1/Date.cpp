#include "Date.h"

struct Date::Impl {
    int year;
    int month;
    int day;
};

Date::Date(int day, std::string month, int year) {
    pimpl = new Date::Impl;
    pimpl->year = year - 1900;
    pimpl->month = monthToInt(month);
    pimpl->day = day - 1;
    
    isValidDate(day, month, year);
}

Date::Date(const Date& date) {
    pimpl = new Date::Impl();
    pimpl->year = date.year() - 1900;
    pimpl->month = monthToInt(date.month());
    pimpl->day = date.day() - 1;
}

Date Date::today() {
    Date today(1, "January", 1900);
    char buffer[1024];
    time_t rawtime;
    time(&rawtime);
    
    strftime(buffer, 1024, "%d %B, %Y", localtime(&rawtime));
    std::stringstream str(buffer);
    
    str >> today;
    return today;
}

Date::~Date() {
    delete pimpl;
}

int Date::day() const {
    return pimpl->day + 1;
}

std::string Date::month() const {
    return intToMonth(pimpl->month);
}

int Date::year() const {
    return pimpl->year + 1900;
}

Date Date::incDays(long days) const {
    int year = pimpl->year;
    int month = pimpl->month;
    int day = pimpl->day;
    
    while (days --> 0) {
        day++;
        
        if (day + 1 > getDays()[month]) {
            if (month == 1 && day == 28 && isLeapYear(year))
                continue;
            
            day = 0;
            month++;
            
            if (month >= 12) {
                year++;
                month = 0;
            }
        }
    }
    
    return Date(day + 1, intToMonth(month), year + 1900);
}

Date Date::incMonths(int months) const {
    int year = pimpl->year;
    int month = pimpl->month + months;
    int day = pimpl->day;
    
    while (month >= 12) {
        year++;
        month -= 12;
    }
    
    if (day > 28 && month == 1)
        day = 27 + isLeapYear(year);
    else if (day + 1 > getDays()[month])
        day = getDays()[month] - 1;
    
    return Date(day + 1, intToMonth(month), year + 1900);
}

Date Date::incYears(int years) const {
    return incMonths(years * 12);
}

Date& Date::operator= (const Date& other) {
    if (this == &other) return *this;
    
    pimpl->year = other.pimpl->year;
    pimpl->month = other.pimpl->month;
    pimpl->day = other.pimpl->day;
}

bool operator== (const Date& a, const Date& b) {
    return  a.year() == b.year()
            && a.month() == b.month()
            && a.day() == b.day();
}

bool operator!= (const Date& a, const Date& b) {
    return !(a == b);
}

bool operator< (const Date& a, const Date& b) {
    // I vehemently hate that this is non-member.
    
    if (a.year() != b.year())
        return a.year() < b.year();
    
    if (monthToInt(a.month()) != monthToInt(b.month()))
        return monthToInt(a.month()) < monthToInt(b.month());
    
    return a.day() < b.day();
}

bool operator<= (const Date& a, const Date& b) {
    return (a < b) || (a == b);
}

bool operator> (const Date& a, const Date& b) {
    return !(a <= b);
}

bool operator>= (const Date& a, const Date& b) {
    return !(a < b);
}

std::ostream& operator<< (std::ostream& stream, const Date& date) {
    stream << date.day() << " " << date.month() << ", " << date.year();
    return stream;
}

std::istream& operator>> (std::istream& stream, Date& date) {
    int day, year, intmonth;
    std::string month;
    
    stream >> day;
    if (stream.fail()) { // recover
        stream.clear();
        stream >> month;
        stream >> month;
        stream >> month;
        stream.setstate(std::ios::failbit);
        throw "Invalid date value.";
    }
    stream >> month;
    if (stream.fail()) throw "Invalid date value.";
    stream >> year;
    if (stream.fail()) {
        stream.clear();
        stream >> month;
        stream.setstate(std::ios::failbit);
        throw "Invalid date value.";
    }
    
    month = month.substr(0, month.length() - 1);
    if (isValidDate(day, month, year))
        date = Date(day, month, year);
    else
        stream.setstate(std::ios::failbit);

    return stream;
}

bool isLeapYear(int year) {
    year += 1900;
    if (year % 400 == 0) return true;
    if (year % 100 == 0) return false;
    return year % 4 == 0;
}

bool isValidDate(int day, std::string month, int year) {
    int intmonth = monthToInt(month);
    day -= 1;
    
    if (year < 1900 || year > 2100)
        throw "Invalid year.";
    if (intmonth >= 12)
        throw "Invalid month.";
    
    int days = getDays()[intmonth];
    int extradays = intmonth == 1 && isLeapYear(year - 1900);
    
    if (day < 0 || day + 1 >  days + extradays)
        throw "Invalid day.";
    
    return true;
}

// These really should be part of the Date class, but since they must be private
// and the operators are written as non-member functions, it has to be done like this.
// *angry face*
std::vector<std::string>& getMonths() {
    static std::vector<std::string> months;
    if (months.size() == 0) {
        months.push_back("January");
        months.push_back("February");
        months.push_back("March");
        months.push_back("April");
        months.push_back("May");
        months.push_back("June");
        months.push_back("July");
        months.push_back("August");
        months.push_back("September");
        months.push_back("October");
        months.push_back("November");
        months.push_back("December");
    }
    
    return months;
}

std::vector<int>& getDays() {
    static std::vector<int> days;
    if (days.size() == 0) {
        days.push_back(31);
        days.push_back(28);
        days.push_back(31);
        days.push_back(30);
        days.push_back(31);
        days.push_back(30);
        days.push_back(31);
        days.push_back(31);
        days.push_back(30);
        days.push_back(31);
        days.push_back(30);
        days.push_back(31);
    }
    
    return days;
}

int monthToInt(std::string month) {
    std::vector<std::string> &months = getMonths();
    return distance(months.begin(), find(months.begin(), months.end(), month));
}

std::string intToMonth(int month) {
    if (month < 0 || month >= 12) throw 0;
    
    return getMonths()[month];
}
