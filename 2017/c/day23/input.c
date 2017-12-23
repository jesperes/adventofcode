#include <stdio.h>

int main() {
  int c = 0;
  int b = 0;
  int a = 0;
  int g = 0;
  int h = 0;
  int f = 0;
  int e = 0;
  int d = 0;
  int mul = 0;

  /*
   * When a == 0, b and c are initialized to 84.
   * When a == 1, b is set to 108400 and c is set to 125400.
   */

  b = 84;         // 1: set b 84
  c = b;          // 2: set c b
  if (a != 0)     // 3: jnz a 2
    goto label1;  // 3: -
  goto label2;    // 4: jnz 1 5
label1:           // 5: mul b 100
  b *= 100;       // 5: -
  mul++;          // mul counter
  b += 100000;    // 6: sub b -100000
  c = b;          // 7: set c b
  c += 17000;     // 8: sub b -17000

  /*
   * end of initialization phase
   */

label2:           // 9: -
  f = 1;          // 9: set f 1
  d = 2;          // 10: set d 2
label5:           // 11: -
  e = 2;          // 11: set e 2
label4:           // 12: -
  g = d;          // 12: set g d
  g = g * e;      // 13: mul g e
  mul++;          // mul counter
  g = g - b;      // 14: sub g b
  if (g != 0)     // 15: jnz g 2
    goto label3;  // 15: -
  f = 0;          // 16: set f 0
label3:           // 17:
  printf("Label3: mul count == %d\n", mul);
  e++;            // 17: sub e -1
  g = e;         // 18: set g e
  g = g - b;     // 19: sub g b
  if (g != 0)    // 20: jnz g -8
    goto label4; // 20:
  d++;           // 21: sub d -1
  g = d;         // 22: set g d
  g = g - b;     // 23: sub g b
  if (g != 0)    // 24: jnz g -13
    goto label5; // 24: -
  if (f != 0)    // 25: jnz f 2
    goto label6; // 25: -
  h++;           // 26: sub h -1
label6:          // 27: -
  g = b;         // 27: set g b
  g = g - c;     // 28: sub g c
  if (g != 0)    // 29: jnz g 2
    goto label7; // 29: -
  goto end;      // 30: jnz 1 3
label7:          // 31: -
  b += 17;       // 31: sub b -17
  goto label2;   // 32: jnz 1 -23

end:
  printf("Mul count: %d\n", mul);
  printf("Value of a == %d\n", a);
  printf("Value of b == %d\n", b);
  printf("Value of c == %d\n", c);
  printf("Value of d == %d\n", d);
  printf("Value of e == %d\n", e);
  printf("Value of f == %d\n", f);
  printf("Value of g == %d\n", g);
  printf("Value of h == %d\n", h);
  return 0;
}
