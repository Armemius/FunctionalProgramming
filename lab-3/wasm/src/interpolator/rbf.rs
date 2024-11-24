use super::{Interpolator, Point};

pub struct RbfInterpolator;

fn gauss_fn(r: f64) -> f64 {
    (-r.powi(2)).exp()
}

fn gen_matrix(points: &[Point]) -> Vec<Vec<f64>> {
    let n = points.len();
    let mut matrix = vec![vec![0.0; n]; n];

    for i in 0..n {
        for j in 0..n {
            matrix[i][j] = gauss_fn((points[i].x - points[j].x).abs());
        }
    }

    matrix
}

// Solve matrix equation Ax = y using Gaussian elimination
fn solve_matrix(matrix: &Vec<Vec<f64>>, y: &Vec<f64>) -> Vec<f64> {
    let n = matrix.len();
    let mut a = matrix.clone();
    let mut b = y.to_vec();

    for i in 0..n {
        let mut max = i;
        for j in i + 1..n {
            if a[j][i].abs() > a[max][i].abs() {
                max = j;
            }
        }

        a.swap(i, max);
        b.swap(i, max);

        for j in i + 1..n {
            let coef = a[j][i] / a[i][i];
            for k in i..n {
                a[j][k] -= coef * a[i][k];
            }
            b[j] -= coef * b[i];
        }
    }

    let mut x = vec![0.0; n];
    for i in (0..n).rev() {
        let mut sum = 0.0;
        for j in i + 1..n {
            sum += a[i][j] * x[j];
        }
        x[i] = (b[i] - sum) / a[i][i];
    }

    x
}

impl Interpolator for RbfInterpolator {
    fn gen_function<'a>(&self, points: &'a [Point]) -> Box<dyn Fn(f64) -> f64 + 'a> {
        let matrix = gen_matrix(points);
        let y = points.iter().map(|p| p.y).collect();
        let w = solve_matrix(&matrix, &y);

        Box::new(move |x: f64| {
            let mut res = 0.0;
            for i in 0..points.len() {
                res += w[i] * gauss_fn((x - points[i].x).abs());
            }
            res
        })
    }

    fn latex_equation(&self, points: &Vec<Point>) -> String {
        let matrix = gen_matrix(points);
        let y = points.iter().map(|p| p.y).collect();
        let w = solve_matrix(&matrix, &y);

        let mut res = "f(x) = ".to_string();
        for i in 0..points.len() {
            res += format!(
                "{:.10} * e^{{-(x - {:.10})^2}}",
                w[i],
                points[i].x
            )
            .as_str();
            if i != points.len() - 1 {
                res += " + ";
            }
        };
        res
    }

    fn points_required(&self) -> std::ops::Range<usize> {
        4..8
    }
}