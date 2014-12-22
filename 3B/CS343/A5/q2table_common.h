// This file contains declarations common to all
// tables, provided as a mixin.

    size_t numPhil;
    Printer &printer;
    
    // Reperesents which forks are currrently on the table
    std::vector<bool> forksAvailable;
    
  public:
    Table(const unsigned int NoOfPhil, Printer &prt);
    void pickup(unsigned int id);
    void putdown(unsigned int id);
  