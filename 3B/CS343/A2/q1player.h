#ifndef Q1_PLAYER_H
#define Q1_PLAYER_H

#include "q1umpire.h"
#include "q1potato.h"

#include <vector>

_Coroutine Player {
  public:
    typedef std::vector<Player*> PlayerList;
  
    Player(Umpire &umpire, unsigned int id, PlayerList &players);
    unsigned int getId() const;
    void toss(Potato &potato);
  
  private:
    void main();
  
    Umpire &umpire;
    unsigned int id;
    PlayerList &players;
  
    // keeps track of the last toss()'d potato
    Potato *potato;
};

#endif
