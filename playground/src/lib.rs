use js_sys::Function;
use nameless::{Environment, Evaluator, Lexer, Parser};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn run_code(code: &str, callback: &Function) -> Result<(), JsValue> {
    let (program, errors) = Parser::new(Lexer::new(&code)).parse_program();
    let env = Environment::new();

    if errors.len() > 0 {
        callback.call1(
            &JsValue::null(),
            &JsValue::from(
                errors
                    .iter()
                    .map(|error| error.to_string())
                    .collect::<Vec<String>>()
                    .join("\n"),
            ),
        )?;
    }

    let evaluator = Evaluator::new(|out: String| {
        match callback.call1(&JsValue::null(), &JsValue::from(out.to_string())) {
            Err(_) => {}
            Ok(_) => {}
        }
    });

    match evaluator.eval(program, &env) {
        Err(error) => {
            callback.call1(&JsValue::null(), &JsValue::from(error.to_string()))?;
            Ok(())
        }
        _ => Ok(()),
    }
}
