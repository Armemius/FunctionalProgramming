// Euler Problem 3: Monolithic solutions
pub(crate) fn solution(n: u64) -> u64 {
    let mut n = n;
    let mut factor = 2;
    while n > 1 {
        if n % factor == 0 {
            n /= factor;
        } else {
            factor += 1;
        }
    }
    factor
}

pub(crate) fn recursive_solution(n: u64) -> u64 {
    fn recursive_solution(n: u64, factor: u64) -> u64 {
        if n == 1 {
            factor
        } else if n % factor == 0 {
            recursive_solution(n / factor, factor)
        } else {
            recursive_solution(n, factor + 1)
        }
    }
    recursive_solution(n, 2)
}

// Euler Problem 3: Map solution
pub(crate) fn map_solution(n: u64) -> u64 {
    (2 .. (n as f64).sqrt() as u64 + 1)
        .filter(|&x| n % x == 0)
        .filter(|&x| (2 .. (x as f64).sqrt() as u64 + 1).all(|y| x % y != 0))
        .last()
        .unwrap()
}

// Euler Problem 3: Infinite list approach
fn prime_numbers() -> impl Iterator<Item = u64> {
    (1..)
        .filter(|&x| (2 .. (x as f64).sqrt() as u64 + 1).all(|y| x % y != 0))
}

pub(crate) fn infinite_list_solution(n: u64) -> u64 {
    prime_numbers()
        .take_while(|&x| x <= n)
        .filter(|&x| n % x == 0)
        .last()
        .unwrap()
}
