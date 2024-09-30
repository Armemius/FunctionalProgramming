pub(crate) fn solution(n: u64) -> u64 {
    (2..=n)
        .step_by(2)
        .flat_map(|x| std::iter::repeat(x).take(4))
        .scan(1, |acc, curr| {
            *acc += curr;
            Some(*acc)
        })
        .fold(1, |acc, x| acc + x)
}
