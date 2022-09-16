package main

import "fmt"

type MyInt int
func (i MyInt) extendo() {}

type MyString string
func (s MyString) extendo() {}

type MyStruct struct {
	str string
	num int
}

func NewMyStruct() *MyStruct {
	return &MyStruct {
		str: "Yay",
		num: 8,
	}
}

type MyInterface interface {MyString|MyInt|MyStruct}

func polymorph[h MyInterface](some h) {
	fmt.Println(some)
}


func main() {
	var s MyString = "henlo"
	var i MyInt = 3
	m := NewMyStruct()

	polymorph(s)
	polymorph(i)
	polymorph(*m)
}
