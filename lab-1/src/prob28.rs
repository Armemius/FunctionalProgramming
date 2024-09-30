pub(crate) fn solution(n: u64) -> u64 {
    (1..(n - 1) / 2 + 1)
        .flat_map(|x| std::iter::repeat(x * 2).take(4))
        .scan(1, |acc, curr| {
            *acc += curr;
            Some(*acc)
        })
        .fold(1, |acc, x| acc + x)
}
