use std::fmt::Result;
use std::io::Result as IoResult;

// Nested path, Bring both the "io" module and the "Write" module into scope
use std::io::{self, Write};

fn function1() -> Result {

}

fn function2() -> IoResult<()> {

}
