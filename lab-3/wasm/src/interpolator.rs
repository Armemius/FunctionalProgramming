use std::ops::Range;

use serde::{Deserialize, Serialize};

pub mod lagrange;
pub mod linear;
pub mod newton;
pub mod rbf;

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }
}

pub trait ToPoint {
    fn to_point(&self) -> Result<Point, String>;
}

impl ToPoint for String {
    fn to_point(&self) -> Result<Point, String> {
        let coords: Vec<&str> = self.trim().split_whitespace().collect();
        if coords.len() != 2 {
            return Err("Invalid input".to_string());
        }
        let x = coords[0]
            .trim()
            .parse::<f64>()
            .map_err(|_| "Invalid x coordinate".to_string())?;
        let y = coords[1]
            .trim()
            .parse::<f64>()
            .map_err(|_| "Invalid y coordinate".to_string())?;
        Ok(Point::new(x, y))
    }
}

pub trait Interpolator {
    fn gen_function<'a>(&'a self, points: &'a [Point]) -> Box<dyn Fn(f64) -> f64 + 'a>;

    fn latex_equation(&self, points: &Vec<Point>) -> String;

    fn points_required(&self) -> Range<usize>;

    fn interpolate_points(&self, points: &Vec<Point>, step: f64) -> Vec<Point> {
        let mut interpolated = Vec::new();

        let required_points = self.points_required();
        let elements = if points.len() > required_points.end {
            &points[points.len() - required_points.end..]
        } else {
            points
        };

        let interpolation_func = self.gen_function(elements);

        let mut it = elements.first().unwrap().x;
        while it < elements.last().unwrap().x + step {
            interpolated.push(Point {
                x: it as f64,
                y: interpolation_func(it as f64),
            });
            it += step;
        }

        interpolated
    }
}

pub fn get_interpolator(method: &str) -> Result<Box<dyn Interpolator>, String> {
    match method {
        "linear" => Ok(Box::new(linear::LinearInterpolator)),
        "lagrange" => Ok(Box::new(lagrange::LagrangeInterpolator)),
        "newton" => Ok(Box::new(newton::NewtonInterpolator)),
        "rbf" => Ok(Box::new(rbf::RbfInterpolator)),
        _ => Err("Unknown interpolation method".to_string()),
    }
}
