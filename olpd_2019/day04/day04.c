#include <stdio.h>
#include <string.h>

int main()
{
  int lower = 402328;
  int upper = 864247;
  char s[7];
  int part1 = 0;
  int part2 = 0;

  for (int i = lower; i <= upper; i++) {
    sprintf(s, "%d", i);

    char a = s[0];
    char b = s[1];
    char c = s[2];
    char d = s[3];
    char e = s[4];
    char f = s[5];

    if (a > b || b > c || c > d || d > e || e > f)
      continue;

    if (a == b || b == c || c == d || d == e || e == f) {
      part1++;

      if ((a == b && b != c) ||
          (a != b && b == c && c != d) ||
          (b != c && c == d && d != e) ||
          (c != d && d == e && e != f) ||
          (d != e && e == f))
        part2++;
    }
  }

  return part1 == 454 && part2 == 288 ? 0 : 1;
}
