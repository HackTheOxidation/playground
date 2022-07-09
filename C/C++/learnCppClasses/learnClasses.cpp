#include <iostream>
using namespace std;

class Rectangle {
	int width, height;

    public:
	Rectangle();
	Rectangle(int, int);
    ~Rectangle();

	int getArea() {
		return (width * height);
	}
};

Rectangle::Rectangle() {
	width = 5;
	height = 5;
}

Rectangle::Rectangle(int a, int b) {
	width = a;
	height = b;
}

int main(void) {

	Rectangle rect = Rectangle(5,3);
	cout << "rect area: " << rect.getArea() << endl;

	return 0;
}
