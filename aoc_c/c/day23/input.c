#include <stdio.h>

int main() {
    int c = 0;
    int b = 0;
    int a = 1;
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

    b = 84; // 1: set b 84
    c = b;  // 2: set c b

    // Part 2 makes things more complicated by increasing the
    // values of b and c
    if (a != 0) {
        b *= 100; // 5: -
        mul++;
        b += 100000; // 6: sub b -100000
        c = b;       // 7: set c b
        c += 17000;  // 8: sub b -17000
    }

    /*
     * end of initialization phase
     */

    /*
     * By looking at the jumps, we see that there are a three nested
     * levels of loops. Each loop exits when g == 0.
     */
#if 0
    do {
        f = 1; // 9: set f 1
        d = 2; // 10: set d 2

        int g0 = g;

        do {
            e = 2; // 11: set e 2

            do {
                g = d * e - b;

                mul++;

                if (g == 0) // 15: jnz g 2
                    f = 0;  // 15: -

                e++; // 17: sub e -1

                g = e - b;
            } while (g != 0);

            d++;       // 21: sub d -1
            g = d - b; // 23: sub g b
        } while (g != 0);

        if (f == 0) // 25: jnz f 2
            h++;    // 26: sub h -1

        g = b - c; // 28: sub g c

        if (g != 0)
            b += 17;

        printf("Iteration in outer loop: h == %d, g == %d (delta-g %d)\n", h, g, g - g0);

    } while (g != 0);
#endif

    h = 1;
    g = -17000;
    do {
        g += 17;
        h++;
        printf("Iteration in outer loop: h == %d, g == %d\n", h, g);
    } while (g != 0);

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
