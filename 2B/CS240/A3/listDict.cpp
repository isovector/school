#include <iostream>
#include <ctime>
#include <cstdlib>
using namespace std;

// This is our List!
template<typename T>
struct List {
    // And here is its templated Node
    struct Node {
        Node(T val) : value(val), prev(NULL), next(NULL) { }
        ~Node() {
            if (next)
                delete next;
        }
        
        T value;
        Node *prev;
        Node *next;
    };
    
    // ------------------------------------------------
    // HEURISTICS
    
    // This is the function signature of h1 and h2
    typedef void (*Heuristic)(List<T>&, Node*);
    
    static void h0(List<T>& list, Node *node) {
    }
    
    static void h1(List<T>& list, Node *node) {
        if (list.head == node)
            return;
        
        if (node->prev)
            node->prev->next = node->next;
        if (node->next)
            node->next->prev = node->prev;
        
        node->next = list.head;
        if (node->next)
            node->next->prev = node;
        
        list.head = node;
    }

    static void h2(List<T>& list, Node *node) {
        list.swap(node, node->prev);
    }
    
    // ------------------------------------------------
    // NOTHING EXCITING
    
    List() : head(NULL), tail(NULL) { }
    
    List(const List& other) : head(other.head), tail(other.tail) { }
    
    ~List() {
        // Deleting the node chain calls into deleting its successor
        delete head;
    }
    
    // ------------------------------------------------
    // LIST METHODS
    
    // Put a value at the end of the list
    void insert(T value) {
        List<T>::Node *node = new List<T>::Node(value);
        
        if (!head)
            head = node;
        if (tail)
            tail->next = node;
        
        node->prev = tail;
        tail = node;
    }
    
    // Swap two nodes
    void swap(Node *a, Node *b) {
        if (!a || !b) return;
        
        T value = a->value;
        a->value = b->value;
        b->value = value;
    }
    
    // Find a node and return the number of searches to get there
    size_t lookup(T value) {
        size_t search = 1;
        
        Node *node = head;
        while (node) {
            if (node->value == value) {
                // We found it - call the desired heuristic
                heuristic(*this, node);
                return search;
            }
            
            ++search;
            node = node->next;
        }
        
        // OH SHEEEEEI---
        return 999999;
    }
    
    // ------------------------------------------------
    // DATA MEMBERS
    
    Heuristic heuristic; // The heuristic to call upon successful lookup
    Node *head;
    Node *tail;
};

// ------------------------------------------------
// DISTRIBUTIONS

// The function signature of distribution generators
typedef int (*distribution)();


int uniform() {
    return rand() % 100 + 1;
}

int linear() {
    // Pick a number between 5050. Sum the numbers up to
    // that. The number that we add that makes it over the random
    // number is the one we want to return!
    int val = rand() % 5050;
    int cur = 0;
    
    for (int i = 0; i < 100; i++) {
        cur += i;
        
        if (cur > val)
            return i + 1;
    }
    
    return 100;
}

int exponential() {
    int i = 101;
    
    while (i --> 1) {
        // We have a 50% chance to have this value
        if (rand() % 2)
            return i;
    }
    
    return 1;
}

// ------------------------------------------------
// MAIN

int main(int argc, char **argv) {
    if (argc != 5) return 1;
    
    // Seed that nugget
    srand(time(0));
    
    // Populate our list
    List<int> list;
    for (int i = 0; i < 100; i++) list.insert(i + 1);
    
    // Choose a heuristic
    list.heuristic =    argv[1][0] == 'n' ? List<int>::h0 :
                        argv[1][1] == '1' ? List<int>::h1 : 
                        List<int>::h2;
    
    // Choose a distribution
    distribution dist = argv[2][0] == 'u' ? uniform : 
                        argv[2][0] == 'l' ? linear : 
                        exponential;
    
    // Train it
    int training = atoi(argv[3]);
    for (int i = 0; i < training; i++)
        list.lookup(dist());
    
    // Try it
    int trials = atoi(argv[4]), totals = 0, lookup;
    for (int i = 0; i < trials; i++) {
        totals += lookup = list.lookup(dist());
        cout << lookup << endl;
    }
    
    return 0;
}