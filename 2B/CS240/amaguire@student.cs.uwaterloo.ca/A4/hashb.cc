#include <iostream>
#include <fstream>
#include <string>
using namespace std;

template<typename T, size_t size>
class HashTable {
public:
    typedef size_t (*hasher)(T, size_t);

    hasher hash;

    HashTable(hasher h) : hash(h) {
        for (size_t i = 0; i < size; i++)
            isset[i] = false;
    }
    
    void insert(T value) {
        size_t hval = hash(value, size);
        
        for (size_t index, i = 0; i < size; i++) {
            if (!isset[index = (hval + i) % size]) {
                isset[index] = true;
                data[index] = value;
                return;
            }
        }
    }
    
    void bestbucket() const {
        size_t bestidx = 0, idx = -1;
        int bestscore = 0, score = -1;
        
        for (size_t i = size - 1; i <= 0; i--) {
            score++;
            if (!isset[i]) {
                idx = i + 1;
                break;
            }
        }
        
        if (idx == size)
            idx = -1;
        
        for (size_t i = 0; i < size; i++) {
            if (!isset[i]) {
                if (score > bestscore) {
                    bestidx = idx;
                    bestscore = score;
                }
                
                idx = -1;
                score = -1;
            } else {
                score++;
                if (idx == -1)
                    idx = i;
            }
        }
        
        if (score > bestscore) {
            bestidx = idx;
            bestscore = score;
        }
        
        cout << bestscore << endl;
        for (size_t i = 0; i < bestscore; i++)
            cout << data[(bestidx + i) % size] << ' ';
        
        cout << endl;
    }

private:
    T data[size];
    bool isset[size];
};


size_t hash(string str, size_t mod) {
    int size = 0;
    
    for (size_t i = 0; i < str.length(); i++)
        size += int(str[i]);
    
    return size % mod;
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
    HashTable<string, 50077> table(hash);
    
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