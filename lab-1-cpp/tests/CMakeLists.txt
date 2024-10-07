include(CTest)
enable_testing()
include(FetchContent)
FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.15.2.zip
        DOWNLOAD_EXTRACT_TIMESTAMP true
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

add_executable(prob3test prob3.cpp)
target_link_libraries(prob3test PRIVATE gtest_main prob3)
add_test(NAME prob3 COMMAND prob3test)

add_executable(prob28test prob28.cpp)
target_link_libraries(prob28test PRIVATE gtest_main prob28)
add_test(NAME prob28 COMMAND prob3test)