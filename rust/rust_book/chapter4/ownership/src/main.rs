fn main() {
    let mut s = String::from("hello");

    s.push_str(", world");

    let s2 = s; // The pointer to s is dropped, s is now out of scope

    println!("{}", s2); // println!("{}", s); - Borrowing after move is not allowed and will not compile

    let s3 = s2.clone(); // Clones the data in s2, by making a new String pointer

    println!("{}, {}", s2, s3); // s3 is a clone of s2, so both are valid and the programme compiles

    let x = 5;
    let y = x; // the value of x gets copied to y, so both remain valid because integers are stored on the stack

    println!("x = {}, y = {}", x, y);

    let s = String::from("hello");

    takes_ownership(s); // The pointer to s is passed on to the "takes_ownership()" function, so s is not longer valid

    makes_copy(x); // x is an integer stored on the stack, so x is copied and remains valid

    println!("x = {}", x);

    let s1 = gives_ownership(); // Returns ownership from a function

    let s2 = String::from("hello");

    let s3 = takes_and_gives_back(s2); // Takes ownership of s2 and returns ownership to s3

    println!("s1 = {}, s3 = {}", s1, s3);


    let (s4, len) = calculate_length(s1);

    println!("The length of '{}' is {}.", s4, len);


    let len2 = len_ref(&s4); // Parses a reference to s2

    println!("The length of '{}' is {}.", s4, len2);


    let mut st = String::from("hello");

    change(&mut st); // Parses st as a mutable reference

    println!("st = {}", st);


    // Mutable references must avoid data race (race condition)
    let mut s = String::from("hello");

    {
        let _r1 = &mut s;
    } // r1 must go out of scope before a new mutable reference can be created

    let r2 = &mut s;

    // Rule: don't mix mutable and immutable references
    println!("r2 = {}", r2); // Ownership is transfered with void functions, so r2 is out of scope

    let _r3 = &s;


    // Slices
    let s = String::from("hello world");

    let hello = &s[0..5]; // References to s with range
    let world = &s[6..11];

    let first = first_word(&s);

    println!("hello = {}, world = {}, first = {}", hello, world, first);

}

//fn first_word(s: &String) -> usize { // Finds the index of the end of the first word in s, but i is not dependent on the state of s = BAD IDEA
  //  let bytes = s.as_bytes();

    //for (i, &item) in bytes.iter().enumerate() {
      //  if item == b' ' {
   //         return i;
        //}
   // }

    //s.len()
//}

fn first_word(s: &String) -> &str { // Finds the first word in s and returns a slice (reference) to it = GOOD IDEA
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}

fn change(some_string: &mut String) { // a mutable reference
    some_string.push_str(", world");
}

fn len_ref(s: &String) -> usize { // Using reference & does not transfer ownership, a reference is immutable by default
    s.len()
}

fn takes_ownership(some_string: String) {
    println!("{}", some_string);
}

fn makes_copy(some_integer: i32) {
    println!("{}", some_integer);
}

fn gives_ownership() -> String {
    let some_string = String::from("hello");

    some_string
}

fn takes_and_gives_back(a_string: String) -> String {
    a_string
}

fn calculate_length(s: String) -> (String, usize) {
    let length = s.len();

    (s, length)
}
