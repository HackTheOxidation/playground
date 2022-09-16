package main

import (
	"fmt"
)

func generateData(n int) []int {
	data := make([]int, n)
	
	for i := 0; i < n; i++ {
		data[i] = 1 + 1
	}
	
	return data
}

func generateChannels(splits int) ([]chan int, []chan int) {
	in := make([]chan int, splits)
	out := make([]chan int, splits)
	
	for i := 0; i < splits; i++ {
		in[i] = make(chan int)
		out[i] = make(chan int)
	}

	return in, out
}

func filter(in *[]int, out chan int, step int, off int) {
	defer close(out)
	for i := off; i < len(*in); i += step {
		data := (*in)[i]
		
		if data % 2 == 0 {
			out <- data
		}
	}
}

func transform(in <-chan int, out chan<- int) {
	defer close(out)
	for data := range in {
		out <- data * 2
	}
}

func reduce(out []chan int) int {
	acc := 0

	for i := range out {
		for d := range out[i] {
			acc += d
		}
	}

	return acc
}


func main() {
	data := generateData(4000)

	splits := 16
	in, out := generateChannels(splits)

	for i := 0; i < splits; i++ {
		go filter(&data, in[i], splits, i)
	}

	for i := 0; i < splits; i++ {
		go transform(in[i], out[i])
	}

	result := reduce(out)
	
	fmt.Printf("Result: %d\n", result)
}
