// Enums contain symbolic values
// And can act like Algebraic data types from functional languages
enum IpAddrKind {
    V4(u8, u8, u8, u8),
    V6(String),
}

struct IpAddr {
    kind: IpAddrKind,
    address: String,
}


// Enums can support multiple variants with different types
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

// Similar to structs impl blocks can also be used for enums
impl Message {
    fn call(&self) {

    }
}

// Using enums as types for function parameters
fn route(ip_kind: IpAddrKind) {}

fn main() {
    // Declaring a variable with an enum type 
    let four = IpAddrKind::V4(127, 0, 0, 1);
    let six = IpAddrKind::V6;

    // Using enums and structs together
    let home = IpAddr {
        kind: four,
        address: String::from("127.0.0.1"),
    };
    
    let m = Message::Write(String::from("hello"));
    m.call();

    // The Option<T> enum is similar to Haskells Maybe type and avoid the Null-reference problem
    let some_number = Some(5);
    let some_string = Some("a string");

    let absent_number: Option<i32> = None;

}
