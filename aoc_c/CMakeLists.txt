cmake_minimum_required(VERSION 3.13)
project(aoc_c LANGUAGES C)
enable_testing()

set(CMAKE_C_FLAGS_DEBUG "-g -Wall -Werror")
set(CMAKE_C_FLAGS_RELEASE "-g -O3 -Wall -Werror")
file(GLOB PUZZLE_SRC src/aoc*_day*.c src/aoc*_day*.cpp)
set(UTILS src/md5.c)

foreach(puzzle ${PUZZLE_SRC})
  get_filename_component(exename ${puzzle} NAME_WE)
  add_executable(${exename} ${puzzle} ${UTILS})
  add_test(NAME ${exename} COMMAND ${exename}
    ${PROJECT_SOURCE_DIR}/inputs/${exename}.txt)
endforeach()
