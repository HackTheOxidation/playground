#pragma once
#include <iostream>

using namespace std;

template <typename T>
class ArrayList {
	public:
		ArrayList(void);
		ArrayList(T arr[], int size);
		void append(T element);
		T get(int index);
		T remove(int index);
		T insert(T element, int index);
		T pop(void);
		int length(void);
		void print(void);

	private:
		T *ptr;
		int size;
};


template <typename T>
ArrayList<T>::ArrayList() {
	this->size = 0;
}

template <typename T>
ArrayList<T>::ArrayList(T arr[], int size) {
	this->ptr = arr;
	this->size = size;
}

template <typename T>
void ArrayList<T>::append(T element) {
	T *old = this->ptr;
	this->size++;
	this->ptr = new T[size];

	for	(int i = 0; i < size-1; i++) {
		this->ptr[i] = old[i];
	}

	this->ptr[size-1] = element;
} 

template <typename T>
T ArrayList<T>::get(int index) {
	if (index >= this->size || index < 0) {
		return NULL;
	}

	return ptr[index];
}

template <typename T>
T ArrayList<T>::remove(int index) {
	if (index >= this->size || index < 0) {
		return NULL;
	}

	T toRemove = this->ptr[index];
	T *old = this->ptr;

	this->ptr = new T[this->size - 1];
	
	for (int i = 0; i < index; i++) {
		this->ptr[i] = old[i];
	}

	for (int i = index + 1; i < this->size; i++) {
		this->ptr[i-1] = old[i];
	}

	this->size--;

	return toRemove;
}

template <typename T>
T ArrayList<T>::insert(T element, int index) {
	if (index >= this->size || index < 0) {
		return NULL;
	}

	T toRemove = this->ptr[index];
	this->ptr[index] = element;

	return toRemove;
}

template <typename T>
T ArrayList<T>::pop() {
	T toRemove = this->ptr[this->size - 1];
	T *old = this->ptr;
	this->size--;
	this->ptr = new T[size];

	for (int i = 0; i < this->size; i++) {
		this->ptr[i] = old[i];
	}

	return toRemove;
}

template <typename T>
int ArrayList<T>::length() {
	return this->size;
}

template <typename T>
void ArrayList<T>::print() {
	cout << "ArrayList: ";
	for(int i = 0; i < this->size; i++) {
		cout << this->ptr[i] << " ";
	}
}
