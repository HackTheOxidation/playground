use std::collections::HashMap;

enum SpreadsheetCell {
    Int(i32),
    Float(f64),
    Text(String),
}

fn main() {
    // Creating a vector struct with the i32 type
    let v: Vec<i32> = Vec::new();

    // Creating a mutable vector struct with "vec!" macro and type inference
    let mut v = vec![1, 2, 3];

    v.push(5);
    v.push(6);
    v.push(7);
    v.push(8);

    {
        let local = vec![1, 2, 3, 4];

        // do stuff with local
    } // <- local goes out of scope and is freed here

    let v = vec![1, 2, 3, 4, 5];

    // Creating a reference/pointer to the third element in v
    let third: &i32 = &v[2];
    println!("The third element is {}", third);

    // Using match on the third element of v and prints it if it exists
    match v.get(2) {
        Some(third) => println!("The third element is {}", third),
        None => println!("There is no third element."),
    }

    
    let v = vec![100, 32, 57];
    for i in &v {
        println!("{}", i);
    }

    let mut v = vec![100, 32, 57];
    for i in &mut v {
        *i += 50;
    }

    let row = vec![
        SpreadsheetCell::Int(3),
        SpreadsheetCell::Text(String::from("blue")),
        SpreadsheetCell::Float(10.22),
    ];

    // Char arrays and strings all support UTF-8 in rust
    // Create a new empty String
    let mut s = String::new();
    
    // Creates a char array "data" with some initial data
    let data = "initial contents";

    // Converts the char array "data" into a string
    let s = data.to_string();

    // The "to_string()"-method can be called directly on a char array
    let s = "initial contents".to_string();

    // Equevalent to the above statement
    let s = String::from("initial contents");

    // Creates a string and appends another string to that
    let mut s = String::from("foo");
    s.push_str("bar");

    // This can be done with variables as well without transferring ownership
    let mut s1 = String::from("foo");
    let s2 = "bar";
    s1.push_str(s2);
    println!("s2 is {}", s2);

    // Concatenates s1 and s2, but s1 is now out of scope
    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2;

    // Concatenates s1, s2 and s3 with some "-" and s1 goes out of scope
    let s1 = String::from("tic");
    let s2 = String::from("tac");
    let s3 = String::from("toe");

    // This is difficult to read
    // let s = s1 + "-" + &s2 + "-" + &s3;

    // Use the "format!" macro for complicated String operations
    // NOTE: s1, s2 and s3 remains in scope here
    let s = format!("{}-{}-{}", s1, s2, s3);

    // String indexing like a char array does NOT WORK
    // use slicing instead as chars form grapheme clusters
    let s1 = String::from("hello");
    let h = &s1[0..1];

    // Iterate over characters in a string with "for"-loop and "chars()"
    for c in s1.chars() {
        println!("{}", c);
    }

    // Iterate over bytes in a string with "for"-loop and "bytes()"
    for b in s1.bytes() {
        println!("{}", b);
    }

    let mut scores = HashMap::new();

    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    let teams = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50];

    let mut scores: HashMap<_, _> =
        teams.into_iter().zip(initial_scores.into_iter()).collect();

    // If a type implements the "Copy" trait values are copied otherwise ownership is transferred
    let field_name = String::from("Favorite color");
    let field_value = String::from("Blue");
    
    let mut map = HashMap::new();
    map.insert(field_name, field_value);

    // Gets a value from a HashMap by using a key
    let mut scores = HashMap::new();

    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    let team_name = String::from("Blue");
    let score = scores.get(&team_name); // NOTE: Returns a result wrapped in "Some", as a corresponding value may not exist for the key in the HashMap

    for (key, value) in &scores {
        println!("{}: {}", key, value);
    }

    // Updating a HashMap
    scores.insert(String::from("Blue"), 25);

    println!("{:?}", scores);

    // Inserting a value only if an entry with that key does not exist in the HashMap
    scores.entry(String::from("Green")).or_insert(50);
    scores.entry(String::from("Blue")).or_insert(50);

    println!("{:?}", scores);

    let text = "hello world wonderful world";

    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }

    println!("{:?}", map);
}
