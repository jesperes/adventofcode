#include <stdio.h>
#include <stdbool.h>
#include <string.h>

bool is_part1(char *buf)
{
  int len = strlen(buf);
  bool has_same = false;
  bool has_decreasing = false;

  for (int i = 0; i < len - 1; i++) {
    char c1 = buf[i];
    char c2 = buf[i+1];

    if (c1 == c2) {
      has_same = true;
    }

    if (c2 < c1) {
      has_decreasing = true;
      break;
    }
  }

  return has_same && !has_decreasing;
}

#define IS_C(N) (buf[N] == c)
#define NOT_C(N) (buf[N] != c)

bool is_part2(char *buf)
{
  for (char c = '0'; c <= '9'; c++) {
    if ((            IS_C(0) && IS_C(1) && NOT_C(2)) ||
        (NOT_C(0) && IS_C(1) && IS_C(2) && NOT_C(3)) ||
        (NOT_C(1) && IS_C(2) && IS_C(3) && NOT_C(4)) ||
        (NOT_C(2) && IS_C(3) && IS_C(4) && NOT_C(5)) ||
        (NOT_C(3) && IS_C(4) && IS_C(5)            )) {
      return true;
    }
  }
  return false;
}

int main()
{
  int start = 402328;
  int end = 864247;
  char buf[10];
  int part1 = 0;
  int part2 = 0;

  for (int n = start; n <= end; n++) {
    sprintf(buf, "%d", n);

    if (is_part1(buf)) {
      part1++;
      if (is_part2(buf))
        part2++;
    }
  }

  return part1 == 454 && part2 == 288 ? 0 : 1;
}
