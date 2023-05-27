fn five() -> i32 {
    5
}

fn plus_one(x: i32) -> i32 {
    x + 1
}

fn main() {
    // Shadowing
    let x = 5;

    let x = x + 1;

    let x = x * 2;

    println!("The value of x is: {}", x);

    // Type conversion with Shadowing
    let spaces = "   ";
    let spaces = spaces.len();

    println!("Number of spaces: {}", spaces);


    // Data types
    let _y: f32 = 3.0;

    let _sum = 5 + 10;

    let _difference = 95.5 - 4.3;

    let _product = 4 * 30;

    let _quotient = 56.7 / 32.2;

    let _remainder = 43 % 5;

    let _t = true;

    let _f: bool = false;

    let _c = 'c';

    // Tuples
    let tup = (500, 6.4, 1);

    let (_n, _m, _p) = tup;

    let five_hundred = tup.0;

    // Arrays
    let _arr = [1, 2, 3, 4, 5];

    let a = [3; 5];

    let first = a[0];

    another_function(five_hundred, first);

    let _block = {
        let x = 3;
        x + 1
    };

    another_function(five(), five());

    let _var = plus_one(5);

    // Control Flow
    let number = 3;

    if number % 5 == 0 {
        println!("number is divisible by 5");
    } else if number % 3 == 0 {
        println!("number is divisible by 3");
    } else {
        println!("number is not divisible by 5 or 3");
    }

    let condition = true;
    let num = if condition { 5 } else { 6 };

    println!("The value of num is: {}", num);

    // The "loop"-loop with return value
    let mut counter = 0;

    let result = loop {
        counter += 1;

        if counter == 10 {
            break counter * 2;
        }
    };

    println!("The result is {}", result);

    // The "while"-loop
    let mut countdown = 3;

    while countdown != 0 {
        println!("{}!", countdown);

        countdown -= 1;
    }

    println!("LIFTOFF!!!");

    // The "for"-loop
    let aa = [10, 20, 30, 40, 50];

    for element in aa.iter() {
        println!("the value is: {}", element);
    }

    for number in (1..4).rev() {
        println!("{}!", number);
    }
    println!("LIFTOFF!!!");

}

// Functions
fn another_function(x: i32, y: i32) {
    println!("The value of x is: {}", x);
    println!("The value of y is: {}", y);
}
