use std::ops::Range;

use super::{Interpolator, Point};

pub struct LinearInterpolator;

impl Interpolator for LinearInterpolator {
    fn gen_function(&self, points: &[Point]) -> Box<dyn Fn(f64) -> f64> {

        // Generate linear function f(x)=kx+b
        let gen_linear_func = |p1: &Point, p2: &Point| {
            let k = (p2.y - p1.y) / (p2.x - p1.x);
            let b = p1.y - k * p1.x;
            move |x: f64| k * x + b
        };

        Box::new(gen_linear_func(&points[0], &points[1]))
    }
    
    fn latex_equation(&self, points: &Vec<Point>) -> String {
        let elements = &points[points.len() - self.points_required().end..];

        let p1 = &elements[0];
        let p2 = &elements[1];

        let k = (p2.y - p1.y) / (p2.x - p1.x);
        let b = p1.y - k * p1.x;

        format!("f(x) = {:.10} * x + {:.10}", k, b)
    }
    
    fn points_required(&self) -> Range<usize> {
        2..2
    }
}
