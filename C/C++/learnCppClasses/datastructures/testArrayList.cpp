#include <iostream>
#include "ArrayList.h"

using namespace std;

int main(void) {

	ArrayList<int> arrayList;
	arrayList.print();
	
	arrayList.append(1);
	arrayList.print();
	
	cout << "\n" << arrayList.get(0) << endl;

	arrayList.append(1);
	arrayList.insert(2, 1);
	arrayList.print();

	cout << "\n" << arrayList.remove(1) << endl;
	cout << arrayList.pop() << endl;

	arrayList.print();
	cout << endl;
	
	return 0;
}
