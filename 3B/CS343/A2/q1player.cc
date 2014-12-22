#include "q1umpire.h"

#include <algorithm>
#include <iostream>
using namespace std;

void Player::main() {
    size_t next;
    
    for ( ;; ) {
        cout << " -> " << getId();
        
        // has the potato exploded?
        if (potato->countdown()) {
            umpire.set(getId());
            
            // early exit from the remainder of the logic
            continue;
        }
        
        // Get our index in the array by subtracting our location from the beginning
        const size_t me = find(players.begin(), players.end(), this) - players.begin();
        do {
            next = rand() % players.size();
        } while (next == me);
        
        players[next]->toss(*potato);
    }
}

Player::Player(Umpire &umpire, unsigned int id, PlayerList &players) :
    umpire(umpire),
    id(id),
    players(players)
{
}

unsigned int Player::getId() const {
    return id;
}

void Player::toss(Potato &potato) {
    // converting a reference to a pointer makes me sad,
    // but there is literally no way to get around this
    // while maintaining the public interface. maybe
    // resume() should accept parameters?
    this->potato = &potato;
    resume();
}