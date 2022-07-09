#pragma once

#include <iostream>
#include <string>

using namespace std;

class Animal {
    protected:
        int legs;
        string name;
        string sound;

    public:
        Animal(void);
        Animal(int, string, string);
        int getLegs(void) const;
        string getName(void) const;
        void setName(string name);
        string getSound(void) const;
        void move(void) const;
};
