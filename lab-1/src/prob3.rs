pub(crate) fn solution(n: u64) -> u64 {
    let mut num = n;
    let mut factor = 2;

    while factor * factor <= num {
        if num % factor == 0 {
            num /= factor;
        } else {
            factor += 1;
        }
    }
    num
}
