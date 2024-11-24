use super::{Interpolator, Point};

pub struct NewtonInterpolator;

fn get_diff(points: &[Point]) -> f64 {
    if points.len() == 1 {
        return points[0].y;
    }

    (get_diff(&points[1..]) - get_diff(&points[..points.len() - 1]))
        / (points.last().unwrap().x - points.first().unwrap().x)
}

impl Interpolator for NewtonInterpolator {
    fn gen_function<'a>(&self, points: &'a [Point]) -> Box<dyn Fn(f64) -> f64 + 'a> {
        let gen_term = |points: &'a [Point], i: usize| {
            move |x: f64| {
                let mut res = 1.0;
                res *= get_diff(&points[..=i]);
                (0..i).for_each(|j| {
                    res *= x - points[j].x;
                });

                res
            }
        };

        let gen_polynomial = |points: &'a [Point]| {
            move |x: f64| {
                let mut res = 0.0;
                (0..points.len()).for_each(|i| {
                    res += gen_term(points, i)(x);
                });

                res
            }
        };

        Box::new(gen_polynomial(points))
    }

    fn latex_equation<'a>(&self, points: &'a Vec<Point>) -> String {
        fn gen_term_eq(i: &usize, points: &[Point]) -> String {
            let mut res = "1.0".to_string();
            res = format!("{} * {:.10}", res, get_diff(&points[..=*i]));
            (0..*i).for_each(|j| {
                res = format!("{} * (x - {})", res, points[j].x);
            });

            res
        }

        fn gen_polynomial_eq(points: &[Point]) -> String {
            let mut res = "0.0".to_string();
            (0..points.len()).for_each(|i| {
                res = format!("{} + ({})", res, gen_term_eq(&i, points));
            });

            res
        }

        gen_polynomial_eq(&points)
    }

    fn points_required(&self) -> std::ops::Range<usize> {
        4..8
    }
}
