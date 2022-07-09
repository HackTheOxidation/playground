use std::thread; // import the std thread lib
use std::time::Duration; // import the Duration module to create delays

fn main() {
    // Spawn a thread from main. Thread function defined as a closure
    // thread::spawn() returns a struct 'JoinHandle'
    let handle = thread::spawn(|| {
        for i in 1..10 {
            println!("hi number {} from the spawned thread!", i);
            thread::sleep(Duration::from_millis(1));
        }
    });

    // If joined here the main thread will wait for the spawned thread to finish

    // A simple loop in the main thread
    for i in 1..5 {
        println!("hi number {} from main thread!", i);
        thread::sleep(Duration::from_millis(1));
    }

    // Wait for the spawned thread to finish
    handle.join().unwrap();

    let v = vec![1, 2, 3];

    // A thread closure can take ownership with the 'move' keyword
    let handle = thread::spawn(move || {
        println!("Here's a vector: {:?}", v);
    });

    handle.join().unwrap();
}
