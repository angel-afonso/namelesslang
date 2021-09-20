#[macro_use]
extern crate pest_derive;

mod compiler;
mod object;
pub mod parser;
mod vm;
pub use object::builtin::builtin_fns;

pub use compiler::{Compiler, Symbol, SymbolTable};
pub(crate) use object::types;
pub use object::Object;
pub use vm::GLOBALS_SIZE;
pub use vm::VM;
