use std::ops::Range;

use super::{Interpolator, Point};

pub struct LagrangeInterpolator;

impl Interpolator for LagrangeInterpolator {
    fn gen_function<'a>(&self, points: &'a [Point]) -> Box<dyn Fn(f64) -> f64 + 'a> {
        let gen_lagrangian_term = |points: &'a [Point], i: usize| {
            move |x: f64| {
                let mut res = 1.0;
                for j in 0..points.len() {
                    if i != j {
                        res *= (x - points[j].x) / (points[i].x - points[j].x);
                    }
                }

                res
            }
        };

        let gen_lagrangian_polynomial = |points: &'a [Point]| {
            move |x: f64| {
                let mut res = 0.0;
                for i in 0..points.len() {
                    res += points[i].y * gen_lagrangian_term(points, i)(x);
                }
                res
            }
        };

        Box::new(gen_lagrangian_polynomial(points))
    }

    fn latex_equation(&self, points: &Vec<Point>) -> String {
        let elements = &points[points.len() - self.points_required().end..];

        let n = elements.len();
        let eq = {
            let mut res = "0.0".to_string();
            for i in 0..n {
                let mut l = "1.0".to_string();
                for j in 0..n {
                    if i != j {
                        l += format!(
                            " * (x - {:.10}) / ({:.10} - {:.10})",
                            elements[j].x, elements[i].x, elements[j].x
                        )
                        .as_str();
                    }
                }
                res += format!(" + {:.10} * ({})", elements[i].y, l).as_str();
            }
            res
        };

        eq
    }

    fn points_required(&self) -> Range<usize> {
        4..4
    }
}

