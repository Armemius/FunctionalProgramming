mod prob3;
mod prob28;

fn main() {
    let number = 600851475143;
    let largest_prime_factor = prob3::map_solution(number);
    println!(
        "Largest prime factor of {} is {}",
        number, largest_prime_factor
    );

    let spiral_size = 1001;
    let sum_diagonals = prob28::map_solution(spiral_size);
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
        let expected = 29;
        let result = prob3::solution(number);
        assert_eq!(result, expected);
        let result = prob3::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob3::map_solution(number);
        assert_eq!(result, expected);
        let result = prob3::infinite_list_solution(number);
        assert_eq!(result, expected);
    }

    #[test]
    fn prob3() {
        let number = 600851475143;
        let expected = 6857;
        let result = prob3::solution(number);
        assert_eq!(result, expected);
        let result = prob3::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob3::map_solution(number);
        assert_eq!(result, expected);
    }

    #[test]
    fn prob28_pre() {
        let number = 5;
        let expected = 101;
        let result = prob28::solution(number);
        assert_eq!(result, expected);
        let result = prob28::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob28::map_solution(number);
        assert_eq!(result, expected);
        let result = prob28::infinite_list_solution(number);
        assert_eq!(result, expected);
    }

    #[test]
    fn prob28() {
        let number = 1001;
        let expected = 669171001;
        let result = prob28::solution(number);
        assert_eq!(result, expected);
        let result = prob28::recursive_solution(number);
        assert_eq!(result, expected);
        let result = prob28::map_solution(number);
        assert_eq!(result, expected);
        let result = prob28::infinite_list_solution(number);
        assert_eq!(result, expected);
    }
}
