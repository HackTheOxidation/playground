use std::fs;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::ErrorKind;

fn main() {
    // Use the "panic!" macro to crash the program
    // panic!("crash and burn");

    // The "File::open()" method returns a "Result<T, E>" enum
    let f = File::open("hello.txt");

    // Use "match" to handle potential errors, depeding on the kind of error, or return the file
    let f = match f {
        Ok(file) => file,
        Err(error) => match error.kind() {
            // If the file does not exist, create it
            ErrorKind::NotFound => match File::create("hello.txt") {
                Ok(fc) => fc,
                Err(e) => panic!("Problem creating the file: {:?}", e),
            },
            other_error => {
                panic!("Problem opening the file: {:?}", other_error)
            }
        },
    };

    // Same as the above, but without using "match".
    // This solution uses closures
    let f = File::open("hello.txt").unwrap_or_else(|error| {
        if error.kind() == ErrorKind::NotFound {
            File::create("hello.txt").unwrap_or_else(|error| {
                panic!("Problem creating the file: {:?}", error);
            })
        } else {
            panic!("Problem opening the file: {:?}", error);
        }
    });

    // Uses "unwrap()" method to extract the file from Resul<T, E> if it exists
    // otherwise it will call panic!
    let f = File::open("hello.txt").unwrap();

    // Same as the above, but parses a message along
    let f = File::open("hello.txt").expect("Failed to open hello.txt");

    
}

// Method that return either the file or an Error struct
// this approach gives more control over what type of error may be returned
fn read_username_from_file() -> Result<String, io::Error> {
    let f = File::open("hello.txt");

    let mut f = match f {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut s = String::new();

    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e),
    }
}


// Same as above, but uses the "?" operator which returns an error if it is encountered
// Propagating errors with "?" uses the "From" trait and calls the "from" method, which automaticly
// converts the result
fn read_username_from_file2() -> Result<String, io::Error> {
    let mut f = File::open("hello.txt")?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}

// Same as above, but shorter and uses "?" cleverly
// NOTE: Don't use "?" directly in the "main" method as "main" returns void
// UNLESS: "main" is defined to return "Result<(), Box<dyn Error>>"
fn read_username_from_file3() -> Result<String, io::Error> {
    let mut s = String::new();

    File::open("hello.txt")?.read_to_string(&mut s)?;

    Ok(s)
}

// Super short version of the above. No boilerplate code here!
fn read_username_from_file4() -> Result<String, io::Error> {
    fs::read_to_string("hello.txt")
}
