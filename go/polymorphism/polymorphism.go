package main

import "fmt"

// This Experiment tries to extend base types with a no-op function
// in order to allow selected base types to share the same interface
// which enables a form of polymorphism that is equal to constrained generics 

type TypeInterface interface {
	noOp()
}

type MyInt int
type MyString string

func (a MyInt) noOp() {}
func (a MyString) noOp() {}

func MyPolymorphicFunc(a TypeInterface) {
	a.noOp()
	fmt.Println(a)
}

func main() {
	var i MyInt = 1
	var s MyString = "hello"

	MyPolymorphicFunc(i)
	MyPolymorphicFunc(s)
}
