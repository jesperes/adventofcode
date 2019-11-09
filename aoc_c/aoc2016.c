#include "aoc2016.h"
#include <stdio.h>

int_solution_t aoc2016_day14(void);

#define RUN_INT(p1, p2, call)                                                      \
  do {                                                                         \
    int_solution_t result = call();                                            \
    if (p1 != result.part1) {                                                  \
      printf("%s part1 incorrect: expected %d, got %d", #call, p1,             \
             result.part1);                                                    \
    }                                                                          \
  } while (0);

int main() {
  RUN_INT(23890, -1, aoc2016_day14);
  return 0;
}
