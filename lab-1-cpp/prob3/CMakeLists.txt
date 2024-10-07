file(GLOB_RECURSE sources CONFIGURE_DEPENDS
    include/*.hpp
    src/*.cpp
)

add_library(prob3 ${sources})
target_include_directories(prob3 PUBLIC include src)
