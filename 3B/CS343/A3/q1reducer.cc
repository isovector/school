#include "q1reducer.h"

#include <map>
using namespace std;

void Reducer::main() {
    // The word counts
    map<string, size_t> wordBuffer;
    
    while (!mappers.empty()) {
        // Flag to determine if any mappers have kvs in their queue
        bool isEmpty = true;
        
        for (vector<Mapper*>::iterator it = mappers.begin(); it != mappers.end(); ++it) {
            Mapper *mapper = *it;
            
            try {
                Mapper::KeyValue kv;
                if (mapper->q->peekFront(&kv)) {
                    // The mapper has elements in its queue
                    isEmpty = false;
                    
                    if (hash(kv.key) % num_reducers == id) {
                        // This kv is ours, so process it!
                        mapper->q->popFront();
                        wordBuffer[kv.key] += kv.value;
                    }
                }
            } catch (KVQueue::EmptyAndClosed eac) {
                // If a KVQueue throws empty and closed, remove the
                // associated mapper from our mapper list, since there
                // is no reason to continue looking at it.
                isEmpty = false;
                mappers.erase(it);
                break;
            }
        }
        
        if (isEmpty) {
            // If no queues have elements, block until we get some more
            signal->P();
        }
    }
    
    // All mappers are now closed, output our results
    for (map<string, size_t>::const_iterator it = wordBuffer.begin(); it != wordBuffer.end(); ++it) {
        osacquire(cout) << it->first << " : " << it->second << endl;
    }
}

Reducer::Reducer(int id, int num_reducers, uSemaphore* signal, const vector<Mapper*> &mappers) :
    id(static_cast<size_t>(id)),
    num_reducers(static_cast<size_t>(num_reducers)),
    signal(signal),
    mappers(mappers)
{
}

Reducer::~Reducer() {
}

unsigned long Reducer::hash(const string& str) {
    unsigned long hash = 5381;
    for (unsigned int i = 0; i < str.size(); ++i) {
        hash = ((hash << 5) + hash) + str[i];
    }
    return hash;
}

int Reducer::getID() {
    return static_cast<int>(id);
}

int Reducer::getNumReducers() {
    return static_cast<int>(num_reducers);
}

uSemaphore* Reducer::getSignal() {
    return signal;
}

vector<Mapper*>& Reducer::getMappers() {
    return mappers;
}
