package aoc2019;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Day 4: Secure Container
 */
public class Day04 {

    int lower = 402328;
    int upper = 864247;

    @Test
    public void solve() throws Exception {
        int part1 = 0;
        int part2 = 0;
        for (int i = lower; i <= upper; i++) {
            char[] s = String.valueOf(i).toCharArray();
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

                if ((a == b && b != c) || //
                        (a != b && b == c && c != d) || //
                        (b != c && c == d && d != e) || //
                        (c != d && d == e && e != f) || //
                        (d != e && e == f))
                    part2++;
            }
        }

        assertEquals(454, part1);
        assertEquals(288, part2);
    }
}
