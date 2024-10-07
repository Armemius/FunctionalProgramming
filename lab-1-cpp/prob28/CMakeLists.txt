file(GLOB_RECURSE sources CONFIGURE_DEPENDS
        include/*.hpp
        src/*.cpp
)

add_library(prob28 ${sources})
target_include_directories(prob28 PUBLIC include src)