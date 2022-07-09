use std::sync::mpsc; // A MessageQueue implementation from the std lib
use std::thread;
use std::time::Duration;

fn main() {
    let (tx, rx) = mpsc::channel(); // Creates a 'channel' (Messagequeue)

    let tx1 = tx.clone(); // Clone the transmitter, creating another producer
    thread::spawn(move || {
        let vals = vec![
            String::from("hi"),
            String::from("from"),
            String::from("the"),
            String::from("thread"),
        ];

        for val in vals {
            tx1.send(val).unwrap(); // Pushes a String to the channel, takes ownership of 'val'
            thread::sleep(Duration::from_secs(1));
        }
    });

    thread::spawn(move || {
        let vals = vec![
            String::from("more"),
            String::from("messages"),
            String::from("for"),
            String::from("you"),
        ];

        for val in vals {
            tx.send(val).unwrap(); // Pushes a String to the channel, takes ownership of 'val'
            thread::sleep(Duration::from_secs(1));
        }
    });

    for received in rx {
        println!("Got: {}", received);
    }
}
