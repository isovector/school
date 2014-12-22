#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
using namespace std;

template<typename T, size_t size>
class HashTable {
public:
    typedef size_t (*hasher)(T, size_t);

    hasher hash;

    HashTable(hasher h, hasher h2) : hash(h), hash2(h2), insertlen(0) {
        for (size_t i = 0; i < size; i++)
            isset[i] = false;
    }
    
    void insert(T value) {
        size_t hval = hash(value, size);
        size_t h2val = hash2(value, size);
        
        for (size_t index, i = 0; i < size; i++) {
            if (!isset[index = (hval + i * h2val) % size]) {
                if (i + 1 > insertlen) {
                    insertlen = i + 1;
                    longestinsert = value;
                }
                    
                
                isset[index] = true;
                data[index] = value;
                return;
            }
        }
    }
    
    void bestbucket() const {
        cout << insertlen << endl;
        size_t hval = hash(longestinsert, size);
        size_t h2val = hash2(longestinsert, size);
        
        for (size_t i = 0; i < insertlen; i++)
            cout << data[(hval + i * h2val) % size] << ' ';
        
        cout << endl;
    }

private:
    hasher hash2;
    T data[size];
    bool isset[size];

    T longestinsert;
    size_t insertlen;
};


size_t hash(string str, size_t mod) {
    int size = 0;
    
    for (size_t i = 0; i < str.length(); i++)
        size += int(str[i]);
    
    return size % mod;
}

size_t hash2(string str, size_t mod) {
    size_t size = 1;
    
    for (size_t i = 0; i < str.length(); i++)
        size += int(str[i]) * pow(3, str.length() - i - 1);
    
    return size % (mod - 1);
}

size_t goodhash(string str, size_t mod) {
    int size = 0;
    
    for (size_t i = 0; i < str.length(); i++)
        if (i % 2)
            size += ~int(str[i] * (i + 1));
        else
            size += int(str[i]);
    
    return size % mod;
}


int main(int argc, char **argv) {
    HashTable<string, 50077> table(hash, hash2);
    
    ifstream file;
    if (argc > 1) {
        if (argv[1][0] == '-' && argv[1][1] == 'n')
            table.hash = goodhash;
        
        file.open(argv[argc - 1]);
    } else
        file.open("/usr/share/dict/words");
    
    string tmp;
    while (file) {
        file >> tmp;
        table.insert(tmp);
    }
    
    table.bestbucket();
    
    return 0; 
}