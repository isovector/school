template <typename T> 
_Task Mergesort {
  public:
    Mergesort(T values[], unsigned int low, unsigned int high, unsigned int depth, T *extra = NULL) :
        values(values),
        extra(extra),
        low(low),
        high(high),
        depth(depth),
        ownsExtra(false)
    {
        // extra is a pointer to our temporary sorting space
        if (!extra) {
            this->extra = new T[high - low];
            ownsExtra= true;
        }
    }
    
    ~Mergesort() {
        if (extra && ownsExtra) {
            delete extra;
        }
    }
    
    void main() {
        partition();
    }
    
    void partition() {
        unsigned int count = high - low;
        
        // There is nothing to partition if we only have one value
        if (count == 1) {
            return;
        }
        
        if (count == 2) {
            // If there are only two values, swap them if they are in the
            // wrong order
            if (values[high - 1] < values[low]) {
                swap(values[low], values[high - 1]);
            }

            return;
        }
        
        // Top is half way through our partition space
        unsigned int top = low + count / 2;
        unsigned int oldLow = low;
        unsigned int oldHigh = high;

        if (depth > 0) {
            // Spawn a child to partition top..high
            Mergesort<T> child(values, top, high, depth - 1, extra);
            
            // Partition from low..top
            high = top;
            partition();
            high = oldHigh;
        } else {
            // This is not exceptionally pretty, but it's impossible
            // to conditionally define the child mergesort in the same
            // scope.
            
            // Partition from low..top
            high = top;
            partition();
            high = oldHigh;
            
            // Partition from top..high
            low = top;
            partition();
            low = oldLow;
        }
        
        // Okay, great! merge them!
        merge(low, top);
    }
    
    void merge(unsigned int a, unsigned int b) {
        const unsigned int count = high - low;
        const unsigned int mid = b;
        
        T *dest = &extra[low];
        
        for (unsigned int i = 0; i < count; ++i, ++dest) {
            // If a < b, or if b is out of values, copy a to dest
            if ((a < mid && values[a] < values[b]) || b == high) {
                *dest = values[a];
                ++a;
            // Otherwise b > a or a is out of values, so copy b
            } else {
                *dest = values[b];
                ++b;
            }
        }
        
        // Move our sorted data back to the main array
        memcpy(&values[low], &extra[low], sizeof(T) * count);
    }
    
  private:
    T *values, *extra;
 
    unsigned int low;
    unsigned int high;
    unsigned int depth;
  
    bool ownsExtra;
};