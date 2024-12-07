mod interpolator;

use interpolator::{Point, ToPoint};
use serde::{Deserialize, Serialize};
use serde_wasm_bindgen::{from_value, to_value};
use wasm_bindgen::prelude::*;

fn check_point_order(points: &Vec<Point>) -> bool {
    points.windows(2).all(|window| match window {
        [p1, p2] => p1.x < p2.x,
        _ => true,
    })
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
    methods: Vec<String>,
    step: f64,
}

#[derive(Debug, Serialize, Deserialize)]
struct InterpolationResult {
    method: String,
    latex_equation: String,
    points: Vec<Point>,
}

#[wasm_bindgen]
pub fn process(data: &JsValue) -> Result<JsValue, JsError> {
    let data: Data = match from_value(data.clone()) {
        Ok(data) => data,
        Err(err) => return Err(JsError::new(&format!("{}", err))),
    };
    if data.step <= 0.0 {
        return Err(JsError::new("Step must be greater than 0.1"));
    }
    if data.points.is_empty() {
        return Err(JsError::new("No points provided"));
    }
    if data.methods.is_empty() {
        return Err(JsError::new("No methods provided"));
    }
    if !check_point_order(&data.points) {
        return Err(JsError::new("Points must be ordered by x increasing"));
    }

    let res = data
        .methods
        .iter()
        .filter_map(|method| {
            let interpolator = interpolator::get_interpolator(method).unwrap();
            if interpolator.points_required().start <= data.points.len() {
                Some(Ok(InterpolationResult {
                    method: method.clone(),
                    latex_equation: interpolator.latex_equation(&data.points),
                    points: interpolator.interpolate_points(&data.points, data.step),
                }))
            } else {
                None
            }
        })
        .collect::<Result<Vec<InterpolationResult>, JsError>>()
        .map(|results| {
            to_value(&results).map_err(|err| JsError::new(&format!("{}", err)))
        });

    match res {
        Ok(res) => res,
        Err(err) => Err(err),
    }
}

use wasm_bindgen_test::wasm_bindgen_test;

#[cfg(target_arch = "wasm32")]
extern crate wasm_bindgen_test;
wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[allow(dead_code)]
#[wasm_bindgen_test]
fn test_process_with_valid_data() {
    let data = Data {
        step: 0.1,
        points: vec![
            // Provide valid Point data here
            Point { x: 0.0, y: 0.0 },
            Point { x: 1.0, y: 1.0 },
        ],
        methods: vec!["linear".to_string()],
    };
    let js_value = to_value(&data).unwrap();
    let result = process(&js_value);
    assert!(result.is_ok());
}

#[allow(dead_code)]
#[wasm_bindgen_test]
fn test_process_with_invalid_step() {
    let data = Data {
        step: 0.0,
        points: vec![
            Point { x: 1.0, y: 1.0 },
            Point { x: 0.0, y: 0.0 },
        ],
        methods: vec!["linear".to_string()],
    };
    let js_value = to_value(&data).unwrap();
    let result = process(&js_value);
    assert!(result.is_err());
}
