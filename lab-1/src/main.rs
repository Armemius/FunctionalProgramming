mod prob3;
mod prob28;

fn main() {
    let number = 600851475143;
    let largest_prime_factor = prob3::solution(number);
    println!(
        "Largest prime factor of {} is {}",
        number, largest_prime_factor
    );

    let spiral_size = 1001;
    let sum_diagonals = prob28::solution(spiral_size);
    println!(
        "Sum of the diagonals in a {}x{} spiral is {}",
        spiral_size, spiral_size, sum_diagonals
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prob3_pre() {
        let number = 13195;
        let result = prob3::solution(number);
        assert_eq!(result, 29);
    }

    #[test]
    fn prob3() {
        let number = 600851475143;
        let result = prob3::solution(number);
        assert_eq!(result, 6857);
    }

    #[test]
    fn prob28_pre() {
        let number = 5;
        let result = prob28::solution(number);
        assert_eq!(result, 101);
    }

    #[test]
    fn prob28() {
        let number = 1001;
        let result = prob28::solution(number);
        assert_eq!(result, 669171001);
    }
}
