mod parser;

use std::any;

use parser::{Point, ToPoint};
use serde::{Deserialize, Serialize};
use serde_wasm_bindgen::{from_value, to_value};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn parse_points(input: String) -> Result<JsValue, JsError> {
    let points = input
        .trim()
        .split('\n')
        .map(|line| line.trim().to_string())
        .map(|line| line.to_point());

    if points.clone().any(|point| point.is_err()) {
        let err = points
            .filter_map(|point| match point {
                Ok(_) => None,
                Err(err) => Some(err),
            })
            .next()
            .unwrap();
        return Err(JsError::new(&format!("{}", err)));
    }

    let data: Vec<Point> = points.map(|point| point.unwrap()).collect();
    to_value(&data).map_err(|err| JsError::new(&format!("{}", err)))
}

#[derive(Debug, Serialize, Deserialize)]
struct Data {
    points: Vec<Point>,
}

#[wasm_bindgen]
pub fn process(data: &JsValue) -> Result<JsValue, JsError> {
    let data: Data = match from_value(data.clone()) {
        Ok(data) => data,
        Err(err) => return Err(JsError::new(&format!("{}", err))),
    };
    log(&format!("{:?}", data));
    Ok(JsValue::NULL)
}
