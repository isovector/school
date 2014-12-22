#ifndef Q1_UMPIRE_H
#define Q1_UMPIRE_H

_Coroutine Umpire;

#include "q1player.h"

_Coroutine Umpire {
  public:
    Umpire(Player::PlayerList &players);
  
    void set(unsigned int player);
    void start();
    
  private:
    void main();
  
    Player::PlayerList &players;
  
    // keeps track of the last set() player
    unsigned int explodedPlayer;
};

#endif
