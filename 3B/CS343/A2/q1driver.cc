#include <time.h>
#include "q1umpire.h"

#include <iostream>
using namespace std;

void uMain::main() {
    int seed = time(NULL);
    size_t numPlayers;
    
    if (argc < 2) {
      usage:
        cout << "Usage: ./hotpotato number-of-players (between 2 and 20) [ random-seed ]" << endl;
        return;
    }
    
    if (argc >= 3) {
        seed = atoi(argv[2]);
    }
    
    srand(seed);
    numPlayers = atoi(argv[1]);
    
    if (numPlayers < 2 || numPlayers > 20) {
        goto usage;
    }
    
    Player::PlayerList players;
    cout << numPlayers << " players in the match" << endl;
    
    Umpire umpire(players);
    for (size_t i = 0; i < numPlayers; ++i) {
        Player *player = new Player(umpire, i, players);
        players.push_back(player);
    }
    
    umpire.start();
}
