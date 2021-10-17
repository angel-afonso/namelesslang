use js_sys::Function;
use nameless::{parser::parse, Compiler, Stream, VM};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub async fn run_code(code: String, callback: Function) -> Result<JsValue, JsValue> {
    match parse(&code, nameless::parser::parser::Mode::REPL) {
        Ok(program) => {
            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile(program) {
                println!("{}", err);
            }

            let mut machine = VM::new(
                compiler.bytecode(),
                Stream::new(
                    |out: String| match callback
                        .call1(&JsValue::null(), &JsValue::from(out.to_string()))
                    {
                        Err(_) => {}
                        Ok(_) => {}
                    },
                    || match callback.call0(&JsValue::null()) {
                        Err(_) => String::new(),
                        Ok(value) => value.as_string().unwrap_or(String::new()),
                    },
                ),
            );

            if let Err(error) = machine.run() {
                return Err(JsValue::from(format!("{}", error)));
            }

            Ok(JsValue::from(format!("{}", machine.last_popped())))
        }
        Err(err) => Err(JsValue::from(format!("{}", err))),
    }
}
