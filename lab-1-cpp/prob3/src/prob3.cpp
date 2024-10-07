#include <prob3.hpp>

auto largest_prime_factor(uint64_t n) -> uint64_t {
    uint64_t largest_factor = 1;
    uint64_t factor = 2;

    while (factor * factor <= n) {
        if (n % factor == 0) {
            largest_factor = factor;
            while (n % factor == 0) {
                n /= factor;
            }
        }
        factor = factor == 2 ? 3 : factor + 2;
    }

    if (n > 1) {
        largest_factor = n;
    }

    return largest_factor;
}
