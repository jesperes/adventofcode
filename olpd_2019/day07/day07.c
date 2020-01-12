#include "assert.h"
#include "intcode.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int64_t run_amplifiers(intcode_t p[5], int s[5], bool with_feedback) {
  // Provide phase settings
  for (int i = 0; i < 5; i++) {
    intcode_execute(&p[i]); // run until first input
    p[i].input = s[i];
    intcode_execute(&p[i]); // consume first input, wait for next
  }

  int64_t output_to_next = 0L;

  do {
    // Feed signal through amplifiers
    for (int i = 0; i < 5; i++) {
      p[i].input = output_to_next;
      intcode_execute(&p[i]);
      output_to_next = p[i].output;
    }
  } while (with_feedback && p[0].last_opcode != OP_END);

  return output_to_next;
}

int all_different(int p1, int p2, int p3, int p4, int p5) {
  if (p1 == p2 || p1 == p3 || p1 == p4 || p1 == p5 || p2 == p3 || p2 == p4 ||
      p2 == p5 || p3 == p4 || p3 == p5 || p4 == p5)
    return false;
  else
    return true;
}

int64_t find_max_thruster(const char *filename, int a, int b,
                          bool with_feedback) {
  int64_t max_thruster = 0L;

  for (int p1 = a; p1 < b; p1++) {
    for (int p2 = a; p2 < b; p2++) {
      for (int p3 = a; p3 < b; p3++) {
        for (int p4 = a; p4 < b; p4++) {
          for (int p5 = a; p5 < b; p5++) {
            if (!all_different(p1, p2, p3, p4, p5))
              continue;

            intcode_t p[5];
            for (int i = 0; i < 5; i++) {
              intcode_init_from_file(&p[i], filename);
            }
            int s[5] = {p1, p2, p3, p4, p5};
            int64_t thruster_level = run_amplifiers(p, s, with_feedback);
            if (thruster_level > max_thruster)
              max_thruster = thruster_level;
          }
        }
      }
    }
  }

  return max_thruster;
}

int main(int argc, char **argv) {
  assert(argc == 2);
  assert(find_max_thruster(argv[1], 0, 5, false) == 70597);
  assert(find_max_thruster(argv[1], 5, 10, true) == 30872528);
  return 0;
}
