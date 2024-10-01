// Euler Problem 28: Monolithic solutions

pub(crate) fn solution(n: u64) -> u64 {
    let mut sum = 1;
    let mut curr = 1;
    for i in 1..=(n - 1) / 2 {
        for _ in 0..4 {
            curr += i * 2;
            sum += curr;
        }
    }
    sum
}

// Euler Problem 28: Recursive solution
pub(crate) fn recursive_solution(n: u64) -> u64 {
    if n == 1 {
        1
    } else {
        solution(n - 2) + 4 * n * n - 6 * (n - 1)
    }
}

// Euler Problem 28: Map solution
pub(crate) fn map_solution(n: u64) -> u64 {
    (2..=n)
        .step_by(2)
        .flat_map(|x| std::iter::repeat(x).take(4))
        .scan(1, |acc, curr| {
            *acc += curr;
            Some(*acc)
        })
        .fold(1, |acc, x| acc + x)
}

// Euler Problem 28: Infinite list approach
fn spiral_diagonals() -> impl Iterator<Item = u64> {
    (2..)
        .step_by(2)
        .flat_map(|x| std::iter::repeat(x).take(4))
}

pub(crate) fn infinite_list_solution(n: u64) -> u64 {
    spiral_diagonals()
        .take(((n / 2) * 4) as usize)
        .scan(1, |acc, curr| {
            *acc += curr;
            Some(*acc)
        })
        .fold(1, |acc, x| acc + x)
}
