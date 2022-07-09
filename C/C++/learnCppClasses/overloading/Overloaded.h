#pragma once

#include <iostream>
#include <string>

class Overloaded {
    public:
        Overloaded(void);
        Overloaded(int, int);
        int getA(void) const;
        int getB(void) const;
        void setA(int);
        void setB(int);
        void print(void) const;
        Overloaded operator+(const Overloaded&);
        Overloaded operator-(const Overloaded&);

    private:
        int a;
        int b;
};
