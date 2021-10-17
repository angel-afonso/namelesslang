mod vm;

#[cfg(test)]
mod test;

pub use vm::{Stream, GLOBALS_SIZE, VM};
