#include <cstddef>

template<typename T>
class RC {
public:
  RC(T* val) : count_(new std::size_t(1)), val_(val)  {
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
    ++(*count_);
  }

  RC& operator=(const RC &rc) {
    val_ = rc.val_;
    count_ = rc.count_;
    ++(*count_);
  }

  T& operator->() {
    return *val_;
  }

private:
  T* val_;
  std::size_t* count_;
};

template<typename T, typename... Args>
RC<T> make_rc(Args... args) {
  return RC<T>(new T(args...));
}
