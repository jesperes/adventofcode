#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

int fuel(int mass)
{
  int f = mass / 3 - 2;
  if (f <= 0)
    return 0;
  else
    return f + fuel(f);
}

int main()
{
  char buf[100];
  int mass1 = 0;
  int mass2 = 0;

  while (true) {
    char *s = fgets(buf, 100, stdin);
    if (s == NULL)
      break;

    int n = atoi(s);
    mass1 += n / 3 - 2;
    mass2 += fuel(n);
  }

  assert(mass1 == 3368364);
  assert(mass2 == 5049684);

  return 0;
}
