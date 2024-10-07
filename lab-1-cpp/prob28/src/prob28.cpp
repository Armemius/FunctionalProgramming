#include <prob28.hpp>

#include <cstddef>

auto spiral_diagonals_sum(const uint64_t size) -> uint64_t {
    uint64_t sum = 1;
    uint64_t current_number = 1;

    for (size_t layer = 1; layer <= size / 2; ++layer) {
        const uint64_t step = layer * 2;
        for (uint64_t corner = 0; corner < 4; ++corner) {
            current_number += step;
            sum += current_number;
        }
    }

    return sum;
}
