#include "q1sortreducer.h"
#include <assert.h>

#include <set>
#include <map>
using namespace std;

void SortReducer::main() {
    // Holds unsubmitted counts of words
    map<string, size_t> wordCounts;
    
    // The last string provided by a given mapper
    map<Mapper*, string> lastString;
    
    while (!mappers.empty()) {
        // Do any mappers have values in their queue?
        bool isEmpty = true;
        
        for (vector<Mapper*>::iterator it = mappers.begin(); it != mappers.end(); ++it) {
            Mapper *mapper = *it;
            
            try {
                Mapper::KeyValue kv;
                if (mapper->q->peekFront(&kv)) {
                    // Peek was successful, so queue is not empty
                    isEmpty = false;
                    
                    if (hash(kv.key) % num_reducers == id) {
                        // This kv is ours to handle!
                        mapper->q->popFront();
                        lastString[mapper] = kv.key;
                        wordCounts[kv.key] += kv.value;
                    }
                }
            } catch (KVQueue::EmptyAndClosed eac) {
                // Queue is empty and closed-- remove it from mappers
                isEmpty = false;
                mappers.erase(it);
                break;
            }
        }
        
        // Have we gotten kvs from all mappers?
        // If so, maybe we can flush some of our wordcount buffer
        if (lastString.size() == mappers.size()) {
            // Calculate the lexigraphically earliest key
            string earliestKey;
            for (map<Mapper*, string>::const_iterator it = lastString.begin(); it != lastString.end(); it++) {
                if (earliestKey.size() == 0 || it->second < earliestKey) {
                    earliestKey = it->second;
                }
            }
            
            // Iterate through our sorted wordbuffer, submitting those which
            // come earlier than the earliest recently accepted kv
            map<string, size_t>::iterator firstValue = wordCounts.begin();
            while (firstValue != wordCounts.end() && firstValue->first < earliestKey) {
                osacquire(cout) << firstValue->first << " : " << firstValue->second << endl;
                
                // This side effect is unavoidable, as we need to erase the current
                // item without invalidating the iterator
                wordCounts.erase(firstValue++);
            }
        }
        
        if (isEmpty) {
            // If no queues have data, block until we get more
            signal->P();
        }
    }
    
    // All mappers are closed-- submit the remaining kvs in the word buffer
    for (map<string, size_t>::const_iterator it = wordCounts.begin(); it != wordCounts.end(); ++it) {
        osacquire(cout) << it->first << " : " << it->second << endl;
    }
}

SortReducer::SortReducer(int id, int num_reducers, uSemaphore* signal, const vector<Mapper*> &mappers) :
    Reducer(id, num_reducers, signal, mappers)
{
}

SortReducer::~SortReducer() {
}
