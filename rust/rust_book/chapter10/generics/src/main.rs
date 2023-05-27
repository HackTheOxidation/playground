use std::cmp::PartialOrd;

/*
fn largest(list: &[i32]) -> &i32 {
    let mut largest = &list[0];

    for item in list {
	if item > largest {
	    largest = item;
	}
    }

    largest
}
*/


fn largest<T: PartialOrd + Copy>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for item in list {
	if item > largest {
	    largest = item;
	}
    }

    largest
}



struct Point<T, U> {
    x: T,
    y: U,
}

impl<T, U> Point<T, U> {
    fn mixup<V, W>(self, other: Point<V, W>) -> Point<T, W> {
	Point {
	    x: self.x,
	    y: other.y,
	}
    }
}

/*
struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
	&self.x
    }
}
*/

fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let result = largest(&number_list);
    println!("The largest number is {}", result);

    let number_list = vec![102, 34, 6000, 89, 54, 2, 43, 8];

    let result = largest(&number_list);
    println!("The largest number is {}", result);

    /*
    let p = Point { x: 5, y: 10 };

    println!("p.x = {}", p.x());
    */

    let p1 = Point { x: 5, y: 10.4 };
    let p2 = Point { x: "Hello", y: 'c' };

    let p3 = p1.mixup(p2);

    println!("p3.x = {}, p3.y = {}", p3.x, p3.y);
}
