/*
 Author: Matthew Might
 Site:   http://matt.might.net/

 This file shows how to create self-inlining
 anonymous closures in C++.

 Anonymous closures are like anonymous functions,
 except that anonymous closures are allowed to
 contain free expressions, whose values are known
 only at run-time.

 For instance, the anonymous function

  $0d * slope + intercept

 captures the values of the free expressions
 `slope' and `intercept'; $0d stands for the first
 argument to the lambda (a double).  In
 mathematics, it is the function f such that:

  f(x) = x*slope + intercept

 If an anonymous closure contains values that are
 known only at runtime, then theses values will be
 passed through the program, from point of
 definition to point of use, as an explicit
 environment structure, while the closure's form
 itself will expanded at compile time at its point
 of use.

 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// At a high-level, we are creating a flexible
// DSEL to capture the structure of computations.

// To make it "pretty," we're using operator
// overloading.

// The core technique is to construct a
// representation of an anonymous computation as
// both a compile-time type and a run-time value.
// Both representations have the structure of a
// syntax tree, but the run-time tree contains
// only values that have to be computed at
// run-time.

// Declarations for syntax tree nodes:

// E1 + E2
template <typename R1, typename R2> struct Sum;

// E1 * E2
template <typename R1, typename R2> struct Prod;

// E1 ^ E2
template <typename R1, typename R2> struct Pow;

// nth argument, takes type T:
template <typename T, int n> struct Arg;

// The base expression type:
template <typename T> struct Exp;

// Definitions for syntax tree nodes:
template <typename R1, typename R2> struct Sum : public Exp<Sum<R1, R2>> {
  const R1 lhs;
  const R2 rhs;
  Sum<R1, R2>(R1 l, R2 r) : lhs(l), rhs(r) {}
};

template <typename R1, typename R2> struct Prod : public Exp<Prod<R1, R2>> {
  const R1 lhs;
  const R2 rhs;
  Prod<R1, R2>(R1 l, R2 r) : lhs(l), rhs(r) {}
};

template <typename R1, typename R2> struct Pow : public Exp<Prod<R1, R2>> {
  const R1 lhs;
  const R2 rhs;
  Pow<R1, R2>(R1 l, R2 r) : lhs(l), rhs(r) {}
};

// An argument to an anonymous function:
template <typename T, int n> struct Arg : public Exp<Arg<T, n>> {};

// Infer is a metaprogram that infers the return
// type of an expression.

// Infer allows programmers to write anonymous
// functions without having to worry about writing
// down the return type of those functions.

template <typename E> struct Infer {
  // If it's not a syntax expression,
  // then it's a literal value, so its
  // type is itself:
  typedef E type;
};

// Arguments carry their type:
template <typename T, int n> struct Infer<Arg<T, n>> { typedef T type; };

// type(double + double) => double
template <> struct Infer<Sum<double, double>> { typedef double type; };

// type(double + int) => double
template <> struct Infer<Sum<double, int>> { typedef double type; };

// type(int + double) => double
template <> struct Infer<Sum<int, double>> { typedef double type; };

// type(int + int) => int
template <> struct Infer<Sum<int, int>> { typedef int type; };

// type(E1 + E2) => type(type(E1) + type(E2))
template <typename E1, typename E2> struct Infer<Sum<E1, E2>> {
  typename Infer<E1>::type typedef E1T;
  typename Infer<E2>::type typedef E2T;
  typename Infer<Sum<E1T, E2T>>::type typedef type;
};

// type(E1 * E2) => type(E1 + E2)
template <typename E1, typename E2> struct Infer<Prod<E1, E2>> {
  typename Infer<Sum<E1, E2>>::type typedef type;
};

// type(E1 ^ E2) => double
template <typename E1, typename E2> struct Infer<Pow<E1, E2>> {
  typedef double type;
};

// Forward declarations for COMPILE.
// COMPILE compiles a type-level syntax tree back into
// an expression, inlining the arguments provided.
template <typename T, typename A>
static inline typename Infer<T>::type COMPILE(const T rt);

template <typename T, typename A0>
static inline typename Infer<T>::type COMPILE(const T rt, A0 arg0);

template <typename T, typename A0, typename A1>
static inline typename Infer<T>::type COMPILE(const T rt, A0 arg0, A1 arg1);

// The base type, Exp<T> contains all the operator
// overloads that make the language pretty:
template <typename T> struct Exp {

  // Operators build up a syntax tree:
  template <typename M> inline Sum<T, M> operator+(M m) {
    (void)m;
    Sum<T, M> rv(*(T *)this, m);
    return rv;
  }

  template <typename M> inline Prod<T, M> operator*(M m) {
    (void)m;
    Prod<T, M> rv(*(T *)this, m);
    return rv;
  }

  template <typename M> inline Pow<T, M> operator^(M m) {
    (void)m;
    Pow<T, M> rv(*(T *)this, m);
    return rv;
  }

  // Application inlines the function:
  inline typename Infer<T>::type operator()() { return COMPILE(*((T *)this)); }

  template <typename A0> inline typename Infer<T>::type operator()(A0 arg0) {
    return COMPILE(*((T *)this), arg0);
  }

  template <typename A0, typename A1>
  inline typename Infer<T>::type operator()(A0 arg0, A1 arg1) {
    return COMPILE(*((T *)this), arg0, arg1);
  }
};

// Operator overloading for syntactic sugar:
// a * exp => exp * a
template <typename T>
static inline Prod<T, double> operator*(double a, Exp<T> e) {
  return e * a;
}

// Inline is a metaprogram that unfolds
// a static closure into its definition.
template <typename T> struct Inline {
  // In the base case, we have a run-time value
  // stored in the struct built by the static
  // closure, so we inline it here.
  static inline T at(const T dynamicValue) { return dynamicValue; }

  template <typename A0> static inline T at(const T dynamicValue, A0 arg0) {
    (void)arg0; // unused
    return dynamicValue;
  }

  template <typename A0, typename A1>
  static inline T at(const T dynamicValue, A0 arg0, A1 arg1) {
    (void)arg0; // unused
    (void)arg1; // unused
    return dynamicValue;
  }
};

// compile($0, x0, ...) => x1
template <typename X1> struct Inline<Arg<X1, 0>> {

  template <typename A0> static inline A0 at(const Arg<X1, 0> rtVal, A0 arg0) {
    (void)rtVal; // unused
    return arg0;
  }

  template <typename A0, typename A1>
  static inline A0 at(const Arg<X1, 0> rtVal, A0 arg0, A1 arg1) {
    (void)rtVal; // unused
    (void)arg1;  // unused
    return arg0;
  }
};

// compile($1, x0, x1, ...) => x1
template <typename X1> struct Inline<Arg<X1, 1>> {

  template <typename A0, typename A1>
  static inline A1 at(const Arg<X1, 1> rtVal, A0 arg0, A1 arg1) {
    (void)rtVal; // unused
    (void)arg0;  // unused
    return arg1;
  }
};

// compile(E1 + E2, ...) =>
//   compile(E1, ...) + compile(E2, ...)
template <typename E1, typename E2> struct Inline<Sum<E1, E2>> {
  typedef Sum<E1, E2> Base;
  typename Infer<Base>::type typedef T;

  static inline T at(const Sum<E1, E2> rtVal) {
    return Inline<E1>::at(rtVal.lhs) + Inline<E2>::at(rtVal.rhs);
  };

  template <typename A0> static inline T at(const Sum<E1, E2> rtVal, A0 arg0) {
    return Inline<E1>::at(rtVal.lhs, arg0) + Inline<E2>::at(rtVal.rhs, arg0);
  };

  template <typename A0, typename A1>
  static inline T at(const Sum<E1, E2> rtVal, A0 arg0, A1 arg1) {
    return Inline<E1>::at(rtVal.lhs, arg0, arg1) +
           Inline<E2>::at(rtVal.rhs, arg0, arg1);
  };
};

// compile(E1 * E2, ...) =>
//   compile(E1, ...) * compile(E2, ...)
template <typename E1, typename E2> struct Inline<Prod<E1, E2>> {
  typedef Prod<E1, E2> Base;
  typename Infer<Base>::type typedef T;

  static inline T at(const Prod<E1, E2> rtVal) {
    return Inline<E1>::at(rtVal.lhs) * Inline<E2>::at(rtVal.rhs);
  };

  template <typename A0> static inline T at(const Prod<E1, E2> rtVal, A0 arg0) {
    return Inline<E1>::at(rtVal.lhs, arg0) * Inline<E2>::at(rtVal.rhs, arg0);
  };

  template <typename A0, typename A1>
  static inline T at(const Prod<E1, E2> rtVal, A0 arg0, A1 arg1) {
    return Inline<E1>::at(rtVal.lhs, arg0, arg1) *
           Inline<E2>::at(rtVal.rhs, arg0, arg1);
  };
};

// compile(E1 ^ E2, ...) =>
//   pow(compile(E1, ...), compile(E2, ...))
template <typename E1, typename E2> struct Inline<Pow<E1, E2>> {
  typedef Pow<E1, E2> Base;
  typename Infer<Base>::type typedef T;

  static inline T at(const Pow<E1, E2> rtVal) {
    return pow(Inline<E1>::at(rtVal.lhs), Inline<E2>::at(rtVal.rhs));
  };

  template <typename A0> static inline T at(const Pow<E1, E2> rtVal, A0 arg0) {
    return pow(Inline<E1>::at(rtVal.lhs, arg0),
               Inline<E2>::at(rtVal.rhs, arg0));
  };

  template <typename A0, typename A1>
  static inline T at(const Pow<E1, E2> rtVal, A0 arg0, A1 arg1) {
    return pow(Inline<E1>::at(rtVal.lhs, arg0, arg1),
               Inline<E2>::at(rtVal.rhs, arg0, arg1));
  };
};

// COMPILE wraps and hides the metaprogram
// invocation that inlines the static closure.
template <typename T, typename A>
static inline typename Infer<T>::type COMPILE(const T rt) {
  return Inline<T>::at(rt);
}

template <typename T, typename A0>
static inline typename Infer<T>::type COMPILE(const T rt, A0 arg0) {
  return Inline<T>::at(rt, arg0);
}

template <typename T, typename A0, typename A1>
static inline typename Infer<T>::type COMPILE(const T rt, A0 arg0, A1 arg1) {
  return Inline<T>::at(rt, arg0, arg1);
}

// Predefined anonymous arguments:
Arg<double, 0> $0d;
Arg<double, 1> $1d;

Arg<int, 0> $0i;
Arg<int, 1> $1i;

// On-the-fly anonymous arguments:

// $<T,position>() : Arg<T,position>
template <typename T, int position> static inline Arg<T, position> $() {
  Arg<T, position> a;
  return a;
}

// $$<T,position>::$ : Arg<T,position>
template <typename T, int position> struct $$ { static Arg<T, position> $; };

// Example; numeric integration, with anonymous functions:
template <typename F>
static inline double INTEGRATE(F f, double a, double b, int n) {
  double delta = (b - a) / n;
  double area = 0.0;
  double x = a;
  for (int i = 0; i < n; ++i, x += delta) {
    double y = f(x);
    area += (y * delta);
  }
  return area;
}

int main(int argc, char *argv[]) {
  (void)argc; // unused
  (void)argv; // unused

  Arg<int, 0> $0;
  Arg<int, 1> $1;

  int i = COMPILE($0 + 10, 30);

  double j = ($1 + 10 + $<double, 0>())(39.0, 50);

  printf("i = %i, j = %lf\n", i, j);

  Arg<double, 0> x;
  double m = 0.37;
  double b = 0.41;

  double A = INTEGRATE(x * m + b, 0.0, 3.0, 2000);

  printf("A = %lf\n", A);

  double A_fast = INTEGRATE(x, 0.0, 4.0, 10000);
  printf("A_fast = %lf\n", A_fast);

  double a = 0.1;
  double c = 0.10;

  double A_nother = INTEGRATE(a * ($0d ^ 2) + (b * $0d) + c, 0.0, 5.0, 100000);

  printf("A_nother = %lf\n", A_nother);

  return EXIT_SUCCESS;
}
