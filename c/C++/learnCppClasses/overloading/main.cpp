#include <iostream>
#include <string>
#include "Overloaded.h"

using namespace std;

int main(void) {

    Overloaded o1;
    o1.print();
    o1.setA(5);
    o1.setB(8);
    o1.print();

    Overloaded o2(2, 3);
    o2.print();

    Overloaded o3 = o1 + o2;
    o3.print();

    Overloaded o4 = o1 - o2;
    o4.print();

    return 0;
}

