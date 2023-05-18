#include <iostream>
#include <string>

#include "subscript.hpp"

void Stupid::operator[](std::string index) {
    std::cout << index << std::endl;
}

