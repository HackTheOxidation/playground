package main

import "fmt"

func printSlice(slice []string) {
	fmt.Printf("Thats a slice for ya: %s\n", slice)
}

func main() {
	slice := []string{}
	slice2 := make([]string, 0)

	printSlice(slice)
	printSlice(slice2)
}
