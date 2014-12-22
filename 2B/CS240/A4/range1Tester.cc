/**
	Simple driver for range1s
	Compile: g++ -D__TESTER__ -o range1 range1Tester.cc range1.cc 
*/
#include "range1.h"
#include <stdio.h>

void tellEdward(const Point& x)
{
	printf("%d\n", x.z);
}

int main()
{
	RangeTree1D t;
	Point x;
	int low;
	int high;

	while(scanf("%d", &x.z) == 1)
		t.add(x);
	scanf("[%d %d]", &low, &high);
	t.rangeSearch(low, high);
}
