pub mod hosting;

mod serving {
    fn take_order() {}

    fn serve_order() {}

    mod back_of_house {
        fn fix_incorrect_order() {
            cook_order();
            super::serve_order();
        }

        fn cook_order() {

        }
    }

    fn take_payment() {}
}
