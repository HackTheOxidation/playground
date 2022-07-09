#include <iostream>
#include <string>
#include "Animal.h"

using namespace std;

Animal::Animal() : legs(0), name("NN"), sound("") {}

Animal::Animal(int legs = 0, string name = "NN", string sound = "") {
    this->legs = legs;
    this->name = name;
    this->sound = sound;
}

int Animal::getLegs() const {
    return this->legs;
}

string Animal::getName() const {
    return this->name;
}

void Animal::setName(string name) {
    this->name = name;
}

string Animal::getSound() const {
    return this->sound;
}

void Animal::move() const {
    cout << this->name << " moved." << endl;
}

