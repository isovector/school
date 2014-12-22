#include <string>
#include <math.h>
#include <vector>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <cctype>
using namespace std;

// Passive data node for hash tables.
struct Node {
    string word;
    Node* next;
};

class HashTable {
  public :
	HashTable ();
	HashTable (int K);
	virtual ~HashTable();
	void insert (string word);
	void remove (string word);	    // You implement this!
	bool lookup (string word) const;
	void print () const;
	void report () const;
	static const int DefaultSize;

  protected : 
	int getTableSize() const;

  private : 
	vector<Node*> table;
	// The "hash" function will be defined by each child class, 
	// each one using a different strategy.  But it needs to be
	// abstract (aka "pure virtual") in the abstract parent class.
	virtual int hash (string key) const = 0;
};

class SmartHashTable : public HashTable {
  public :
	SmartHashTable();
	SmartHashTable(int K);
	virtual ~SmartHashTable() {}

  private:
    virtual int hash(string key) const;
};

class SimpleHashTable : public HashTable {
  public :
	SimpleHashTable();
	SimpleHashTable(int K);
	virtual ~SimpleHashTable() {}

  private:
    virtual int hash(string key) const;
};

// Note that insert/lookup are completely defined here (and call hash) even
// tho hash is not defined here.  This is called the Template Method design
// pattern.  The hash function is private, meaning the children can't call
// it.  However, they can *define* it.  In fact, that's the only difference
// between the children classes: they each provide a difference
// implementation of hash!l


int nextLowerPrime(int top) {
    if (top < 2) return 2;
    for (int j = top; j >= 2; j--) {
        if (j % 2 == 0) continue;
        for (int i = 3; i * i <= j; i+= 2) {
            if (j % i == 0)
                goto nextloop;
        }
        return j;
    nextloop:;
    }
}

const int HashTable::DefaultSize = 1000;
HashTable::HashTable() : table(DefaultSize) {}
HashTable::HashTable(int K) : table(K) {}

SmartHashTable::SmartHashTable() : HashTable() {}
SmartHashTable::SmartHashTable(int k) : HashTable(k) {}

int SmartHashTable::hash(string key) const {
        unsigned long hash = 33;
        const char *str = key.c_str();

        while (int c = *str++)
            hash = (hash << 5) + hash + c;
        return hash % nextLowerPrime(getTableSize());
}

SimpleHashTable::SimpleHashTable() : HashTable() {}
SimpleHashTable::SimpleHashTable(int k) : HashTable(k) {}
int SimpleHashTable::hash(string key) const {
    int val = 0;
    for (int i = 0; i < key.length(); i++)
        val += key[i];
    return val % getTableSize();
}

// The destructor has lots of garbage to clean up!  For each bucket with an
// overflow list, we delete the nodes.
HashTable::~HashTable(){
    for (int i=0; i<getTableSize(); i++) {
	Node* p = table[i];
        while (p!=NULL) {
            Node* temp = p;
            p = p->next;
            delete temp;
        }
    }
}

// Simple accessor function that will be used by children classes to
// implement the hash function.  It's protected as children need to use it
// but not public as external clients do not need to know about it.
// This is the only information about the table that the children classes
// need to know, so we can make the table itself private!
int HashTable::getTableSize() const {
    return (int) table.size();
}

// Use open hashing with unsorted linked list for overflow.
void HashTable::insert(string key) {
    const int slot = hash(key);
    Node* newNode = new Node;
    newNode->word = key;
    newNode->next = table[slot];
    table[slot] = newNode;
}

bool HashTable::lookup (string key) const {
    const int slot = hash(key);
    Node* curNode = table[slot];
    while (curNode != NULL) {
        if (curNode->word == key) {
            return true;
        }
        curNode = curNode -> next;
    }
    return false;
}

// You implement this!
void HashTable::remove(string key) {
    const int slot = hash(key);
    Node* curNode = table[slot];
    Node* del;

    if (curNode && curNode->word == key) {
        del = curNode;
        table[slot] = curNode->next;
        delete del;
        return;
    }

    while (curNode && curNode->next) {
        if (curNode->next->word == key) {
            del = curNode->next;
            curNode->next = curNode->next->next;
            delete del;
            return;
        }
        curNode = curNode -> next;
    }

    return;
}

// To help you debug, if you find it useful.
void HashTable::print() const {
    for (int i=0; i<getTableSize(); i++) {
	if (table[i] != NULL) {
	    Node* p = table[i];
	    while (p != NULL) {
		cout << i << "    " << p->word << endl;
		p = p->next;
	    }
	}
    }
}

// So we can tell how good your hash function is ...
// Do NOT override this function in the version you submit.  We will check!
void HashTable::report () const {
    // K is number of buckets
    const int K = getTableSize();
    // How many overflow elements in each bucket?
    vector<int> stats (K);
    int totalNumEntries = 0;
    int numNonZeros = 0;
    int maxOverflowSize = 0;
    for (int i=0; i<K ; i++) {
        if (table[i] != NULL) {
            numNonZeros++;
            int numEntriesInThisBucket = 0;
            Node* p = table[i];
            while (p != NULL) {
                p = p->next;
                numEntriesInThisBucket++;
            }
            totalNumEntries += numEntriesInThisBucket;
            if (numEntriesInThisBucket > maxOverflowSize) {
                maxOverflowSize = numEntriesInThisBucket;
            }
            stats[i]=numEntriesInThisBucket;
        }
    }
    sort(stats.begin(), stats.end());
    const int numEmptyBuckets = K - numNonZeros;
    const int firstNonZeroBucketIndex = numEmptyBuckets;
    cout << "Number of entries in table: " << totalNumEntries << endl;
    cout << "Total # buckets: " << K << " of which " << numEmptyBuckets 
	<< " (" << 100 * numEmptyBuckets / K << "%)" << " were empty." <<
	endl;
    // We want the avg and median # of elements, but ignoring the empty
    // buckets, on the grounds that if we're looking for a valid word, it
    // doesn't matter how many empty buckets there are, as we'll be hashing
    // to a non-empty bucket and traversing that list.
    // To compute the median, find the index that is half-way between
    // firstNonZeroBucketIndex and K.
    const int median = stats[firstNonZeroBucketIndex + numNonZeros/2];
    const int average = totalNumEntries / numNonZeros;
    cout << "Overflow list length:  Max = " << maxOverflowSize 
	<< "  Median = " << median << "  Average = " << average <<  endl;
}

vector<string> addChar (const vector<string>& v, char c) {
    vector<string> ans;
    for (int i = 0; i < (int) v.size(); i++)
        ans.push_back(c + v.at(i));
    return ans;
}

vector<string> powerset (string s) {
    vector<string> ans;
    if (s.size() == 0)
        ans.push_back("");
    else {
        char c = s.at(0);
        string rest = s.substr(1);
        vector<string> psetRest = powerset(rest);
        ans = addChar(psetRest, c);
        ans.insert(ans.end(), psetRest.begin(), psetRest.end());
    }
    
    return ans;
}

int scrabbleValue(char c) {
    c = tolower(c);
    switch (c) {
        case 'e':
        case 'a':
        case 'i':
        case 'o':
        case 'n':
        case 'r':
        case 't':
        case 'l':
        case 's':
        case 'u':
            return 1;
        case 'd':
        case 'g':
            return 2;
        case 'b':
        case 'c':
        case 'm':
        case 'p':
            return 3;
        case 'f':
        case 'h':
        case 'v':
        case 'w':
        case 'y':
            return 4;
        case 'k':
            return 5;
        case 'j':
        case 'x':
            return 8;
        case 'q':
        case 'z':
            return 10;
    }

    return 0;
}

int scrabbleValue(string word) {
    int count = 0;
    for (int i = 0; i < word.length(); i++)
        count += scrabbleValue(word[i]);
    return count;
}

int main(int argc, char *argv[]) {
    ifstream file;
    SmartHashTable sh(100000);

    if (argc < 2) {
        cerr << "Error, no word list file name provided." << endl;
        exit(1);
    }
    
    string buffer;
    //cin >> buffer;
    file.open(argv[1]);
    
    if (file) {
        while (file >> buffer)
            sh.insert(buffer);
        while (cin >> buffer) {
            vector<string> pset = powerset(buffer);
            string best = "";
            int bestscore = -0xFFFFFF, score;
            for (vector<string>::iterator iter = pset.begin(); iter < pset.end(); iter++) {
                string permute = *iter;
                
                sort(permute.begin(), permute.end());
                do
                    if (sh.lookup(permute)) {
                        score = scrabbleValue(permute);
                        if (bestscore < score) {
                            best = permute;
                            bestscore = score;
                        }
                    }
                while (next_permutation(permute.begin(), permute.end()));
            }
            
            cout << buffer << ": ";
            if (best != "")
                cout << best << " has score of " << bestscore << endl;
            else
                cout << "no matches" << endl;
        }
        //sh.report();
    } else {
        cerr << "Error, couldn't open word list file." << endl;
        exit(1);
    }
}
