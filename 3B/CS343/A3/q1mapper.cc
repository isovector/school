#include "q1mapper.h"
using namespace std;

void Mapper::main() {
    string word;
    while (stream >> word) {
        q->pushBack(KeyValue(word, 1));
        
        while (!signal->empty())
        {
            // Wake up any blocked reducers
            signal->V();
        }
    }
    
    q->close();
    while (!signal->empty())
    {
        // Wake up any blocked reducers
        signal->V();
    }
}

Mapper::Mapper(const string& filename, int queue_len, uSemaphore* signal) :
    q(new KVQueue(queue_len)),
    signal(signal),
    filename(filename),
    stream(filename.c_str())
{
}

Mapper::~Mapper() {
    delete q;
}

uSemaphore* Mapper::getSignal() {
    return signal;
}

const string& Mapper::getFilename() {
    return filename;
}
