#include "q1sortmapper.h"

#include <set>
#include <string>
#include <map>
using namespace std;

// Converts a reverse iterator to a forward iterator, because
// we can't erase rits for some reason.
template <class T>
typename T::iterator_type reverse_to_forward(T rit)
{
    return --rit.base();
}

void SortMapper::main() {
    // Keys we've already sent off to the reducers
    set<string> processedKeys;
    
    // Candidate buffer for kvs to submit on this pass
    map<string, size_t> wordBuffer; 
    do {
        // Clear kvs from last iteration
        wordBuffer.clear();
        
        // Reset stream to the beginning
        stream.clear();
        stream.seekg(0);
        
        string word;
        while (stream >> word) {
            // If we haven't processed this key before, or we are already processing
            // it, it is a candidate for counting.
            if (!processedKeys.count(word) || wordBuffer.count(word)) {
                // If the buffer is full and doesn't contain this word, it is a
                // candidate to replace a word already in the buffer
                if (wordBuffer.size() == buffer_size && !wordBuffer.count(word)) {
                    if (processedKeys.count(word)) {
                        // The key is already processed - no need to continue
                        continue;
                    }
                    
                    // Compare this word to the lexigraphically largest in the buffer
                    map<string, size_t>::iterator it = reverse_to_forward(wordBuffer.rbegin());
                    if (word >= it->first) {
                        // If it is larger, we will not process it
                        continue;
                    }

                    // This word should be in the buffer - make room for it
                    wordBuffer.erase(it);
                }
                
                // Count the word =)
                wordBuffer[word] += 1;
            }
        }
        
        // The stream is complete. Submit the buffer to the reducers.
        for (map<string, size_t>::const_iterator it = wordBuffer.begin(); it != wordBuffer.end(); ++it) {
            // This key has now been processed
            processedKeys.insert(it->first);
            q->pushBack(KeyValue(it->first, it->second));
            
            while (!signal->empty()) {
                // Wake up any reducers who might be blocked on data
                signal->V();
            }
        }
    } while (!wordBuffer.empty());
    
    q->close();
    while (!signal->empty()) {
        // Wake up any reducers who might be blocked on data
        signal->V();
    }
}

SortMapper::SortMapper(const string& filename, int queue_len, int buffer_size, uSemaphore* signal) :
    Mapper(filename, queue_len, signal),
    buffer_size(static_cast<size_t>(buffer_size))
{
}

SortMapper::~SortMapper() {
}
