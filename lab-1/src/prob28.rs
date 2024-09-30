pub(crate) fn solution(n: u64) -> u64 {
    let mut total_sum = 1;
    let mut current_value = 1;
    let mut step = 2;

    for _ in (3..=n).step_by(2) {
        for _ in 0..4 {
            current_value += step;
            total_sum += current_value;
        }
        step += 2;
    }

    total_sum
}
