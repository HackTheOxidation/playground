use std::thread;
use std::time::Duration;

fn simulated_expensize_calculation(intensity: u32) -> u32 {
    println!("calculating slowly...");
    thread::sleep(Duration::from_secs(2));
    intensity
}

fn main() {
    let simulated_user_specified_value = 10;
    let simulated_random_number = 7;

    generate_workout(simulated_user_specified_value, simulated_random_number);
}

// First iteration: multiple unneccesary calls to algorithm == BAD
/*
fn generate_workout(intensity: u32, random_number: u32) {
    if intensity < 25 {
        println!(
            "Today, do {} pushups!",
            simulated_expensize_calculation(intensity)
        );
        println!(
            "Next, do {} situps!",
            simulated_expensize_calculation(intensity)
        );
    } else {
        if random_number == 3 {
            println!("Take a break today! Remember to stay hydrated!");
        } else {
            println!(
                "Today, run for {} minutes!",
                simulated_expensize_calculation(intensity)
            );
        }
    }
}
*/

// Second iteration: only one call to algorithm, but call is forced in all cases == better, but
// still bad
/*
fn generate_workout(intensity: u32, random_number: u32) {
    let expensize_result = simulated_expensize_calculation(intensity);
    if intensity < 25 {
        println!(
            "today, do {} pushups!", expensize_result
        );
        println!(
            "next, do {} situps!", expensize_result
        );
    } else {
        if random_number == 3 {
            println!("take a break today! remember to stay hydrated!");
        } else {
            println!(
                "today, run for {} minutes!", expensize_result
            );
        }
    }
}
*/
  
// Third iteration: using closures, but still calls the algorithm on demand
/*
fn generate_workout(intensity: u32, random_number: u32) {
    let expensive_closure = |num| {
        println!("calculating slowly...");
        thread::sleep(Duration::from_secs(2));
        num
    };

    if intensity < 25 {
        println!(
            "today, do {} pushups!", expensive_closure(intensity)
        );
        println!(
            "next, do {} situps!", expensive_closure(intensity)
        );
    } else {
        if random_number == 3 {
            println!("take a break today! remember to stay hydrated!");
        } else {
            println!(
                "today, run for {} minutes!", expensive_closure(intensity)
            );
        }
    }
}
*/

// Implement struct to capture Lazy Evaluation
struct Cacher<T>
where
    T: Fn(u32) -> u32,
{
    calculation: T,
    value: Option<u32>,
}

// All closures implement the Fn trait, so must the Cacher struct
impl<T> Cacher<T>
where
    T: Fn(u32) -> u32,
{
    fn new(calculation: T) -> Cacher<T> {
        Cacher {
            calculation,
            value: None,
        }
    }

    fn value(&mut self, arg: u32) -> u32 {
        match self.value {
            Some(v) => v,
            None => {
                let v = (self.calculation)(arg);
                self.value = Some(v);
                v
            }
        }
    }
}

// Final iteration: Value is stored in struct and only calculated once == Great!
fn generate_workout(intensity: u32, random_number: u32) {
    let mut expensive_result = Cacher::new(|num| {
        println!("calculating slowly...");
        thread::sleep(Duration::from_secs(2));
        num
    });

    if intensity < 25 {
        println!("Today, do {} pushups!", expensive_result.value(intensity));
        println!("Next, do {} situps!", expensive_result.value(intensity));
    } else {
        if random_number == 3 {
            println!("Take a break today! Remember to stay hydrated!");
        } else {
            println!("Today, run for {} minutes!", expensive_result.value(intensity));
        }
    }
}
