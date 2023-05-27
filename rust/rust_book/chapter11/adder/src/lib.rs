pub fn add_two(a: i32) -> i32 {
    a + 2
}

pub fn greeting(name: &str) -> String {
    format!("Hello {}!", name)
}

pub struct Guess {
    value: i32,
}

impl Guess {
    pub fn new(value: i32) -> Guess {
	if value < 1 {
	    panic!("Guess value must be greater than or equal to 1, got {}.", value);
	} else if value > 100 {
	    panic!("Guess value must be less than or equal to 100, got {}.", value);
	}

	Guess { value }
    }
}

// Private function
fn internal_adder(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    // Rust's privacy rules allows testing private functions
    #[test]
    fn internal() {
	assert_eq!(4, internal_adder(2, 2));
    }

    // The Result<Ok, Err> type can also be used in place of assert!
    #[test]
    fn it_works() -> Result<(), String> {
	if 2 + 2 == 4 {
	    Ok(())
	} else {
	    Err(String::from("two plus two does not equal four"))
	}
    }

    // This test should panic! with a specific message 
    #[test]
    #[should_panic(expected = "Guess value must be less than or equal to 100")]
    fn greater_than_100() {
	Guess::new(200);
    }

    // This test uses a custom message which will be displayed upon failure
    #[test]
    fn greeting_contains_name() {
	let result = greeting("Carol");
	assert!(
	    result.contains("Carol"),
	    "Greeting did not contain name, value was '{}'",
	    result
	);
    }

    // This test is ignored unless Rust is explicitly told to run it
    #[test]
    #[ignore]
    fn exploration() {
        assert_eq!(2 + 2, 4);
    }

    // It is expected that this test should crash with panic!
    #[test]
    #[should_panic]
    fn another() {
	panic!("Make this test fail");
    }

    // This test uses assert_eq! to test if two arguments are equal
    #[test]
    fn it_adds_two() {
	assert_eq!(4, add_two(2));
    }

    // This test expects can_hold() to return true
    #[test]
    fn larger_can_hold_smaller() {
	let larger = Rectangle {
	    width: 8,
	    height: 7,
	};
	let smaller = Rectangle {
	    width: 5,
	    height: 1,
	};

	assert!(larger.can_hold(&smaller));
    }

    // Test a method with arguments where it should return false
    #[test]
    fn smaller_cannot_hold_larger() {
	let larger = Rectangle {
	    width: 8,
	    height: 7,
	};
	let smaller = Rectangle {
	    width: 5,
	    height: 1,
	};

	assert!(!smaller.can_hold(&larger));
    }
}

struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
	self.width > other.width && self.height > other.height
    }
}
