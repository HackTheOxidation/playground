#include <cstdio>
#include <iostream>
#include <string>

using namespace std;

template<typename T>
class RC {
public:
  RC(T* val) : val_(val) {
    count_ = new int(1);
  }
  ~RC() {
    if (--(*count_) == 0) {
      delete val_;
      delete count_;
    }
  }
  RC(const RC &rc) {
    val_ = rc.val_;
    count_ = rc.count_;
    (*count_)++;
  }

private:
  T* val_;
  int* count_;
};
