cmake_minimum_required(VERSION 3.12)
project(aoc_c LANGUAGES C)
enable_testing()
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_C_FLAGS "-g -O3 -Wall -Werror")
file(GLOB PUZZLE_SRC src/aoc*_day*.c src/aoc*_day*.cpp)
set(UTILS src/md5.c)

foreach(puzzle ${PUZZLE_SRC})
  get_filename_component(exename ${puzzle} NAME_WE)
  add_executable(${exename} ${puzzle} ${UTILS})

  set(INPUT ${PROJECT_SOURCE_DIR}/inputs/${exename}.txt)
  if (EXISTS ${INPUT})
    add_test(NAME ${exename} COMMAND ${exename} ${INPUT})
  else()
    add_test(NAME ${exename} COMMAND ${exename})
  endif()
endforeach()

add_executable(intcode_ascii src/intcode.c)
target_compile_definitions(intcode_ascii PUBLIC INTCODE_ASCII_MAIN=1)

add_executable(intcode src/intcode.c)
target_compile_definitions(intcode PUBLIC INTCODE_MAIN=1)
