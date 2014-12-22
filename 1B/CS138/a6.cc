#include <vector>
#include <string>
#include <iostream>
using namespace std;

class Animal {
  public:
    virtual ~Animal() { }
    virtual void speak() const = 0;
  protected:
    Animal(string n) : name(n) { }

    virtual string getName() const {
        return this->name;
    }
  private:
    string name;
};

class Dog : public Animal {
  public:
    Dog(string n) : Animal(n) { }
    ~Dog() { }

    virtual void speak() const {
        cout << "    Dog " << this->getName() << " says \"woof\".\n";
    }
};

class Sheep : public Animal {
  public:
    Sheep(string n) : Animal(n) { }
    ~Sheep() { }

    virtual void speak() const {
        cout << "    Sheep " << this->getName() << " says \"baaa\".\n";
    }
};

class Flock {
  public:
    Flock(string dogName) {
        dog = new Dog(dogName);
    }

    virtual ~Flock() {
        delete dog;
        for (int i = 0; i < sheepList.size(); i++)
            delete sheepList[i];
    }

    virtual void addSheep(string name) {
        sheepList.push_back(new Sheep(name));
    }

    virtual void soundOff() const {
        cout << "The flock of " << sheepList.size() << " sheep speaks!\n";
        dog->speak();
        for (int i = 0; i < sheepList.size(); i++)
            sheepList[i]->speak();
        cout << endl;
    }
  private:
    Dog *dog;
    vector<Sheep*> sheepList;
};

int main (int argc, char* argv[]) {
Flock *myFlock = new Flock ("Spot");
myFlock->soundOff();
myFlock->addSheep ("Daisy");
myFlock->addSheep ("Clover");
myFlock->addSheep ("Estelle");
myFlock->soundOff();
delete myFlock;
myFlock = new Flock ("Rover");
myFlock->addSheep ("Butch");
myFlock->addSheep ("Jonno");
myFlock->soundOff();
// myFlock will die anyway when the program ends, but it's
// still good form to delete all objects you create via "new"
delete myFlock;
}
