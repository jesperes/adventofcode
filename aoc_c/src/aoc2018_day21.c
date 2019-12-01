#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

int64_t R[6] = { 0 };

#define R0 R[0]
#define R1 R[1]
#define R2 R[2]
#define R3 R[3]
#define R5 R[5]

#define ARRAY_SIZE 64 * 1024

int main()
{
  // For part2, keep track of R1 values we have already seen
  int64_t seen_r1[ARRAY_SIZE] = { 0 };
  int next_r1_index = 0;

  // Part1: What is the lowest value for R0 causing the program to
  // stop in as few steps as possible?
  R0 = 0;

  // Part 2: what is the lowest value for R0 causing the program to
  // stop in as many steps as possible? The R1 values will eventually
  // repeat, so we are looking for the first time we see a repeated R1,
  // and take the value immediately before that.

  // outer loop
  do {

    R2 = R1 | 65536;         /* bori 1 65536 2 */
    R1 = 8725355;            /* seti 8725355 6 1 */

  pc08:

    R5 = R2 & 255;           /* bani 2 255 5 */
    R1 += R5;                /* addr 1 5 1 */
    R1 &= 16777215;          /* bani 1 16777215 1 */
    R1 *= 65899;             /* muli 1 65899 1 */
    R1 &= 16777215;          /* bani 1 16777215 1 */

    // R1 is not touched in the inner loop. The only purpose of the
    // inner loop is to produce new values for R2 and R5 which are
    // used to compute R1 above.

    if (R2 >= 256) {
      R5 = 0;                  /* seti 0 0 5 */

      while (1)
        {
          R3 = R5 + 1;          /* addi 5 1 3 */
          R3 *= 256;            /* muli 3 256 3 */

          if (R3 > R2)
            break;

          R5++;
        }

      R2 = R5;                 /* setr 5 1 2 */
      goto pc08;               /* seti 7 6 4 */
    }

    if (next_r1_index == 0) {
      printf("Part 1 solution: %ld\n", R1);
      assert(R1 == 4682012);
    }

    for (int i = 0; i < next_r1_index; i++) {
      if (seen_r1[i] == R1) {
        printf("Part 2 solution: %ld\n", seen_r1[next_r1_index - 1]);
        assert(seen_r1[next_r1_index - 1] == 5363733);
        return 0;
      }
    }

    seen_r1[next_r1_index++] = R1;

    if (next_r1_index >= ARRAY_SIZE) {
      printf("Array size is too small: %d\n", ARRAY_SIZE);
      abort();
    }

  } while (R1 != R0);
}
