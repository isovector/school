#include <vector>
#include <fstream>
#include <sstream>
#include <cstring>
#include <iostream>
#include <cmath>

#ifdef RUN_TESTS
#include <ctime>
#include <cstdlib>
#endif

using namespace std;

int cmp = 0;

template<typename T>
void partition(vector<T>& A, int l, int u, int &k) {
    int part = l;
    for (int i = l + 1; i <= u; i++)
        if (++cmp, A[i] < A[l])
            swap(A[i], A[++part]);
            
    swap(A[l], A[part]);
    k = part;
}

template<typename T>
T quickselect(vector<T>& A, int l, int u, int m, bool alfredo = false, int *key = NULL) {
    int p;
    if (l < u) {
        if (!alfredo)
            p = u;
        else {
            int s = sqrt(u - l);
            quickselect(A, l, l + s - 1, m / (u + 1) * s, true, &p);
        }
        
        swap(A[p], A[l]);
        partition(A, l, u, p);
        
        if (m < p)
            return quickselect(A, l, p-1, m, alfredo, key);
        else if (m > p)
            return quickselect(A, p+1, u, m, alfredo, key);
        
        if (key) *key = p;
        return A[p];
    }
    
    if (key) *key = l;
    return A[l];
}


#ifndef RUN_TESTS
int main(int argc, char *argv[]) {
    bool alfredo = strcmp(argv[1], "--alfredo") == 0;
    
    fstream file(argv[2]);
    string line;
    int val;
    
    if (file)
        getline(file, line);
    
    stringstream str(line);
    vector<int> array;
    while (str && !str.eof()) {
        str >> val;
        array.push_back(val);
    }
        
    file >>  val;

    cout << quickselect(array, 0, array.size() - 1, val - 1, alfredo) << endl;
    cout << cmp << endl;
    

    return 0;
}
#endif

#ifdef RUN_TESTS
int main(int argc, char *argv[]) {
    stringstream args(argv[1]);
    
    srand(time(NULL));
   
    int sample;
    args >> sample;
    
    vector<int> classic;
    int classiccmp = 0;
    vector<int> alfredo;
    int alfredocmp = 0;
    
    int val = 5;
    
    cout << "Classic\tAlfredo" << endl;
    for (int t = 0; t < 100; t++) {
        classic.clear();
        alfredo.clear();
        
        for (int i = 0; i < sample; i++) {
            int r = rand() % sample;
            classic.push_back(r);
            alfredo.push_back(r);
        }
        
        cmp = 0; quickselect(classic, 0, sample - 1, val, false);
        cout << cmp << "\t"; classiccmp += cmp;
        cmp = 0; quickselect(alfredo, 0, sample - 1, val, true);
        cout << cmp << "\n"; alfredocmp += cmp;
    }
    
    return 0;
}
#endif
