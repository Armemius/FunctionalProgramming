use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Point {
    pub x: f64,
    pub y: f64
}

impl Point {
    fn new(x: f64, y: f64) -> Point {
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
        let x = coords[0].trim().parse::<f64>().map_err(|_| "Invalid x coordinate".to_string())?;
        let y = coords[1].trim().parse::<f64>().map_err(|_| "Invalid y coordinate".to_string())?;
        Ok(Point::new(x, y))
    }
}