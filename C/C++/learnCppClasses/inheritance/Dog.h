#pragma once

#include <iostream>
#include <string>
#include "Animal.h"

using namespace std;

class Dog : public Animal {
    private:
        string furColor;
        string breed;

    public:
        string getFurColor(void) const;
        string getBreed(void) const;
};
