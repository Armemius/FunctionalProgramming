#include <gtest/gtest.h>
#include <prob3.hpp>

TEST(Prob3, InitialCheck) {
    ASSERT_EQ(largest_prime_factor(13195), 29);
}

TEST(Prob3, ActualCheck) {
    ASSERT_EQ(largest_prime_factor(600851475143), 6857);
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
