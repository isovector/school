#include <iostream>
using namespace std;

int main() {
    int to_factor = 71531;
    for (int i = 3; i * i <= to_factor; i += 2)
        if (to_factor % i == 0) {
            cout << "Factors are " << i << " and " 
                 << (to_factor / i) << endl;
            return 0;
        }
    return 1;
}