#include <stdio.h>

int numdays(int year) {
    if (year % 400 == 0)
        return 1;
    if (year % 100 == 0)
        return 0;
    return year % 4 == 0 ? 1 : 0;
}


int dayOfYear(int day, int month, int year) {
    int months[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    int doty = 0, i; 

    if (day > 31 || day < 1 || month > 12 || month < 1 || year < 1583) return -1;
    if (months[month - 1] < day && numdays(year) == 0 || (month == 2 && day == 29 && numdays(year) == 0)) return -1;
    

    for (i = 0; i < month - 1; i++)
        doty += months[i];
    doty += day;

    if (doty > 60) doty += numdays(year);

    return doty;
}