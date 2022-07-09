mod front_of_house;

// use brings the entire hosting module into scope
// use crate::front_of_house::hosting;
// Using only one function/method within the module this is idiomatic
// instead:
use crate::front_of_house::hosting::add_to_waitlist;

// "use" works with both absolute and relative paths
// use self::front_of_house::hosting;

// use "pub" to re-export module
pub use crate::front_of_house::hosting;

mod back_of_house {
    pub struct Breakfast {
        pub toast: String,
        seasonal_fruit: String,
    }

    impl Breakfast {
        pub fn summer(toast: &str) -> Breakfast {
            Breakfast {
                toast: String::from(toast),
                seasonal_fruit: String::from("peaches"),
            }
        }
    }

    pub enum Appetizer {
        Soup,
        Salad,
    }
}

pub fn eat_at_restaurant() {
    // Absolute path to function in module
    crate::front_of_house::hosting::add_to_waitlist();

    // Relative path to function in module
    front_of_house::hosting::add_to_waitlist();

    // Making structs in modules public
    let mut meal = back_of_house::Breakfast::summer("Rye");
    meal.toast = String::from("Wheat");
    println!("I'd like {} toast please", meal.toast);

    // Note: the attribute "seasonal_fruit" is still private
    let order1 = back_of_house::Appetizer::Soup;
    let order2 = back_of_house::Appetizer::Salad;

    // Because of the "use" statement public functions, enums and structs from hosting can now be accessed
    hosting::add_to_waitlist();
}

