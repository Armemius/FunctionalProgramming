pub(crate) fn solution(n: u64) -> u64 {
    (2 .. (n as f64).sqrt() as u64 + 1)
        .filter(|&x| n % x == 0)
        .filter(|&x| (2 .. (x as f64).sqrt() as u64 + 1).all(|y| x % y != 0))
        .last()
        .unwrap()
}
