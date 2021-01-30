use js_sys::Function;
use nameless::{
    evaluator::{Environment, Evaluator},
    Lexer, Parser,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub async fn run_code(code: String, callback: Function) -> Result<(), JsValue> {
    let (program, errors) = Parser::new(Lexer::new(&code)).parse_program();
    let env = Environment::new();

    if errors.len() > 0 {
        return Err(JsValue::from(
            errors
                .iter()
                .map(|error| error.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
        ));
    }

    let evaluator = Evaluator::new(
        |out: String| match callback.call1(&JsValue::null(), &JsValue::from(out.to_string())) {
            Err(_) => {}
            Ok(_) => {}
        },
        || match callback.call0(&JsValue::null()) {
            Err(_) => String::new(),
            Ok(value) => value.as_string().unwrap_or(String::new()),
        },
    );

    match evaluator.eval(program, &env) {
        Err(error) => Err(JsValue::from(error.to_string())),
        _ => Ok(()),
    }
}
