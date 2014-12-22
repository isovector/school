#include "q1umpire.h"
#include "q1potato.h"

#include <iostream>
#include <algorithm>
using namespace std;

Umpire::Umpire(Player::PlayerList &players) :
    players(players),
    explodedPlayer(-1)
{
}

void Umpire::set(unsigned int player) {
    explodedPlayer = player;
    resume();
}

void Umpire::start() {
    resume();
}

void Umpire::main() {
    Potato potato;
    
    int round = 1;
    while (players.size() > 1) {
        potato.reset();
        
        cout << "Set " << round << ":\tU";
        players[rand() % players.size()]->toss(potato);
        cout << " is eliminated" << endl;
        
        // track down the exploded player and remove him from play
        for (Player::PlayerList::iterator it = players.begin(); it != players.end(); ++it) {
            if ((*it)->getId() == explodedPlayer) {
                delete *it;
                players.erase(it);
                break;
            }
        }
        
        round++;
    }
    
    cout << players[0]->getId() << " wins the Match!" << endl;
    delete players[0];
}