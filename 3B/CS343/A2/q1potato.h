#ifndef Q1_POTATO_H
#define Q1_POTATO_H

class Potato {
  public:
    Potato(unsigned int maxTicks = 10);
    void reset(unsigned int maxTicks = 10);
    bool countdown();
    
  private:
    unsigned int ticks;
};

#endif
