#include <stdexcept>
#include <iostream>
#include <exception>

using namespace std;

int main(void) {
    try {
        throw 20;
    } catch (int e) {
        cout << "You caught the exception! Congratulations!" << endl;
    }

    return 0;
}

