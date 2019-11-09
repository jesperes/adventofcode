project(aoc_c LANGUAGES C)

set(CMAKE_C_FLAGS_DEBUG "-g -Wall -Werror")
set(CMAKE_C_FLAGS_RELEASE "-g -O3 -Wall -Werror")

add_executable(aoc2016
  md5.c
  aoc2016.c
  aoc2016_day14.c)
