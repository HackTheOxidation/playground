#include <iostream>
#include <string>
#include "Overloaded.h"

using namespace std;

Overloaded::Overloaded() : a(0), b(0) {}

Overloaded::Overloaded(int a, int b) {
    this->a = a;
    this->b = b;
}

int Overloaded::getA() const {
    return this->a;
}

int Overloaded::getB() const {
    return this->b;
}

void Overloaded::setA(int a) {
    this->a = a;
}

void Overloaded::setB(int b) {
    this->b = b;
}

Overloaded Overloaded::operator+(const Overloaded& o1) {
    Overloaded o2;
    o2.setA(this->a + o1.getA());
    o2.setB(this->b + o1.getB());
    return o2;
}

Overloaded Overloaded::operator-(const Overloaded& o1) {
    Overloaded o2;
    o2.setA(this->a - o1.getA());
    o2.setB(this->b - o1.getB());
    return o2;
}

void Overloaded::print() const {
    cout << "Overloaded: " << endl;
    cout << "a = " << this->getA() << endl;
    cout << "b = " << this->getB() << endl;
}
