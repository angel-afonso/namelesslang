use js_sys::Function;
use nameless::{parser::parse, Compiler, VM};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub async fn run_code(code: String, callback: Function) -> Result<JsValue, JsValue> {
    match parse(&code, nameless::parser::parser::Mode::REPL) {
        Ok(program) => {
            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile(program) {
                println!("{}", err);
            }

            let mut machine = VM::new(compiler.bytecode());

            if let Err(error) = machine.run() {
                return Err(JsValue::from(format!("{}", error)));
            }

            Ok(JsValue::from(format!("{}", machine.last_popped())))
        }
        Err(err) => Err(JsValue::from(format!("{}", err))),
    }
}
