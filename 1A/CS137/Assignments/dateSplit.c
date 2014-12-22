#include <stdio.h>

int numdays(int year) {
    if (year % 400 == 0)
        return 1;
    if (year % 100 == 0)
        return 0;
    return year % 4 == 0 ? 1 : 0;
}

int dateSplit(int dayOfYear, int year, int *day, int *month) {
    int months[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }, i = 0;
    *month = 1;
    *day = 0;

    if (dayOfYear == 60 && numdays(year) == 1) {
        *day = 29;
        *month = 2;
        return 1;
    }

    if (dayOfYear > 60 && numdays(year) == 1) dayOfYear--;

    if (dayOfYear < 1 || dayOfYear > 365 + numdays(year) || year < 1583) return 0;
    while (dayOfYear > months[i] && i < 11) {
        dayOfYear -= months[i++];
        ++*month;
    }
    *day = dayOfYear;
    return 1;
}

/*void testDateSplit(int dayOfYear, int year) {
  int day, month;

  if (dateSplit (dayOfYear, year, &day, &month))
    printf ("%d,%d => day:%d, month:%d\n", dayOfYear, year, day, month);
  else
    printf ("%d,%d => invalid\n", dayOfYear, year);
}

int main (void) {
  testDateSplit (59, 2007);
  testDateSplit (60, 2007);
  testDateSplit (61, 2007);
  testDateSplit (59, 2008);
  testDateSplit (60, 2008);
  testDateSplit (61, 2008);
  

  return 0;
}*/