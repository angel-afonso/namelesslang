mod code;
mod compiler;

use code::make;
pub use code::{read_be_u16, Instructions, OpCode};
pub use compiler::{Bytecode, Compiler};
