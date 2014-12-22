#ifndef Q2PRINTER_H
#define Q2PRINTER_H

#include <vector>

_Monitor Printer {
    void preamble(size_t elves) const;
    void flush();
    
  public:
    enum States { Starting = 'S', Blocked = 'B', Unblocked = 'U', Finished = 'F', // general
          Napping = 'N', Awake = 'A',             // Santa
          Working = 'W', NeedHelp = 'H',          // elf
          OnVacation = 'V', CheckingIn = 'I',     // reindeer
          DeliveringToys = 'D', DoneDelivering = 'd', // Santa, reindeer
          Consulting = 'C', DoneConsulting = 'c', // Santa, elves
          ConsultingFailed = 'X',                 // elf
        
        None = ' '
    };
    
    Printer(const size_t MAX_NUM_ELVES);
    void print(size_t id, States state);
    void print(size_t id, States state, size_t numBlocked);
    
  private:
    struct StateNum {
        StateNum(States state, size_t num) : state(state), num(num) { }
        
        States state;
        size_t num;
    };
  
    std::vector<StateNum> states;
};

#endif
