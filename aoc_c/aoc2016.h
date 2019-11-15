#ifndef AOC2016_H_
#define AOC2016_H_

#include <time.h>

#define aoc_assert(msg, test)                                                  \
  do {                                                                         \
    if (!(test)) {                                                             \
      printf("%s:%d: [%s] assertion '%s' failed\n", __FILE__, __LINE__, msg,   \
             #test);                                                           \
      return -1;                                                               \
    } else {                                                                   \
      printf("%s:%d: [%s] success\n", __FILE__, __LINE__, msg);                \
    }                                                                          \
  } while (0)

#define NANO_SECS 1000000000

#define timespec_to_seconds(t) ((t).tv_sec + ((double)(t).tv_nsec) / NANO_SECS)

#define aoc_puzzle(F)                                                          \
  do {                                                                         \
    struct timespec start, stop;                                               \
    printf("Running %s...\n", #F);                                             \
    clock_gettime(CLOCK_REALTIME, &start);                                     \
    F();                                                                       \
    clock_gettime(CLOCK_REALTIME, &stop);                                      \
    double elapsed = timespec_to_seconds(stop) - timespec_to_seconds(start);   \
    printf("Finished %s in %g seconds.\n", #F, elapsed);                       \
  } while (0)

#endif /* AOC2016_H_ */
