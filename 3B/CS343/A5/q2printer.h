#ifndef Q2_PRINTER_H
#define Q2_PRINTER_H

#include <string>
#include <vector>

_Monitor Printer {
  public:
    // Moved this in from Philosopher, to simplify header includes.
    enum States { None = ' ', Thinking = 'T', Hungry = 'H', Eating ='E', Waiting = 'W', Finished = 'F' };    
  
    Printer( unsigned int NoOfPhil );
    void print( unsigned int id, States state );
    void print( unsigned int id, States state, unsigned int bite, unsigned int noodles );
    
  private:
    // structure which represents (state,bites,noodles)
    class StateNum {
        States state;
        bool hasNums;
        size_t numA;
        size_t numB;
        
      public:
        explicit StateNum(States state);
        explicit StateNum(States state, size_t numA, size_t numB);
      
        // Note: there are no operator!= s
        bool operator==(States state);
        bool operator==(const StateNum &other);
        std::string getString() const;
    };
    
    void preamble(size_t elves) const;
    void flush();
    void printRaw(size_t id, StateNum state);
    
    // The current state values for each philosopher
    std::vector<StateNum> states;
};

#endif
