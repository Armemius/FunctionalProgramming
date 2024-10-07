#include <gtest/gtest.h>
#include <prob28.hpp>

TEST(Prob28, InitialCheck) {
    ASSERT_EQ(spiral_diagonals_sum(5), 101);
}

TEST(Prob28, ActualCheck) {
    ASSERT_EQ(spiral_diagonals_sum(1001), 669171001);
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}