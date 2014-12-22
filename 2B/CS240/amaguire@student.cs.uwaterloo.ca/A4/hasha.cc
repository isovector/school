#include <iostream>
#include <fstream>
#include <string>
using namespace std;

template<typename T, size_t size>
class HashTable {
public:
    typedef size_t (*hasher)(T, size_t);

    hasher hash;

    HashTable(hasher h) : hash(h), bucketest(0) {
        for (size_t i = 0; i < size; i++) {
            data[i] = NULL;
            bucket[i] = 0;
        }
    }
    
    void insert(T value) {
        size_t hval = hash(value, size);
        
        if (++bucket[hval] > bucket[bucketest])
            bucketest = hval;
        
        Node **node = &data[hval];
        while (*node)
            node = &(*node)->next;
        
        *node = new Node(value);
    }
    
    void bestbucket() const {
        cout << bucket[bucketest] << endl;
        Node *node = data[bucketest];
        while (node) {
            cout << node->value << ' ';
            node = node->next;
        }
        
        cout << endl;
        
        size_t empty = 0;
        for (size_t i = 0; i < size; i++)
            empty += bucket[i] == 0;
        cout << empty << endl;
    }

private:
    struct Node {
        Node(T v) : value(v), next(NULL) { }
        
        operator T() const {
            return value;
        }
        
        T value;
        Node *next;
    };

    Node *data[size];
    
    size_t bucket[size];
    size_t bucketest;
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
    HashTable<string, 10000> table(hash);
    
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