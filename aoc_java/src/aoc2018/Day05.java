package aoc2018;

import static java.lang.Character.toUpperCase;
import static java.lang.Math.abs;
import static java.lang.Math.min;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import common.AocPuzzle;

public class Day05 extends AocPuzzle {
    public Day05() {
        super(2018, 5);
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals(10496, react(getInputAsCharArray()));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(5774, part2(getInputAsCharArray()));
    }

    private int part2(char[] input) {
        int minLen = Integer.MAX_VALUE;
        char[] buf = new char[input.length];

        /*
         * Check each polymer type, remove them one by one, and see which one
         * yields the shortest reacted string.
         */
        for (char c = 'a'; c <= 'z'; c++) {
            char C = toUpperCase(c);
            int j = 0;

            for (int i = 0; i < input.length; i++) {
                char x = input[i];
                if (x != c && x != C)
                    buf[j++] = x;
            }

            minLen = min(minLen, react(buf, j));
        }

        return minLen;
    }

    static private int react(char[] buf) {
        return react(buf, buf.length);
    }

    static private int react(char[] buf, int l) {
        while (true) {
            int l0 = react_once(buf, l);

            if (l0 == l)
                return l0;
            else
                l = l0;
        }
    }

    static private int react_once(char[] buf, int len) {

        int src = 0;
        int tgt = 0;

        while (true) {
            if (src == len)
                break;

            if (src + 1 == len) {
                buf[tgt++] = buf[src++];
                break;
            }

            char x = buf[src];
            char y = buf[src + 1];

            if (abs(x - y) == 32) {
                src += 2;
            } else {
                buf[tgt++] = buf[src++];
            }
        }

        return tgt;
    }
}
