cmake_minimum_required(VERSION 3.15)

project(FunctionalProgrammingLab1
        VERSION 1.0
        DESCRIPTION "C++ implementation of Euler project problems №3 and №28"
        LANGUAGES C CXX
)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_subdirectory(prob3)
add_subdirectory(prob28)
add_subdirectory(tests)
