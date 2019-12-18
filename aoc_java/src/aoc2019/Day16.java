package aoc2019;

import static java.lang.Math.abs;
import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 16: Flawed Frequency Transmission
 */
public class Day16 extends AocPuzzle {

    int[] segments = new int[] { 0, 1, 0, -1 };

    public Day16() throws IOException {
        super(2019, 16);
    }

    /**
     * Implementation of FFT used for part 1.
     *
     * @param s       Input String
     * @param repeats Number of repetitions
     * @return
     */
    String fft(String s, int repeats) {
        int[] in = new int[s.length()];
        int[] out = new int[s.length()];
        int[] tmp;
        for (int i = 0; i < s.length(); i++) {
            in[i] = s.charAt(i) - '0';
        }

        // Outer loop, once for each application of FFT
        for (int n = 0; n < repeats; n++) {
            // Loop over all the position in the output string.
            for (int j = 0; j < out.length; j++) {
                out[j] = 0;
                // Inner loop over all positions in the input string
                for (int k = 0; k < in.length; k++) {
                    int p = pattern(k, j + 1);
                    out[j] += p * in[k];
                }

                out[j] = abs(out[j]) % 10;
            }

            // Swap the arrays before repeating.
            tmp = in;
            in = out;
            out = tmp;
        }

        // When we are done, output is in "in", because of the final swap.
        return toString(in);
    }

    String toString(int[] out) {
        char[] c = new char[out.length];
        for (int i = 0; i < c.length; i++) {
            c[i] = (char) (out[i] + '0');
        }
        return new String(c);
    }

    /**
     * Get the pattern for the given position
     *
     * @param i The position in the pattern sequence
     * @param j The length of the segments
     */
    int pattern(int i, int j) {
        return segments[((i + 1) / j) % 4];
    }

    String part1(String input) {
        return fft(input, 100).substring(0, 8);
    }

    /**
     * Part 2. The quadratic complexity of the general part 1 solution is
     * not usable here.
     *
     * Key observation 1: The answer for part 2 is determined by an offset
     * which tells us where in the final output string we should take the
     * answer. This is taken from the first 7 digits of the input string.
     *
     * Key observation 2: Due to how the pattern sequence looks, out[i] only
     * depends on digits in[i] ... in[last]. This means that we can ignore
     * all leading digits up to the desired message offset (the pattern
     * sequence is always 0 for these numbers).
     *
     * Key observation 3: Message offset is larger than half the (repeated)
     * input length, which means that the pattern sequence for the digits we
     * care about are like this:
     *
     * @formatter:off
     *
     * a b c d e f g h
     * 1 1 1 1 1 1 1 1 -> h + g + f + e + d + c + b + a
     *   1 1 1 1 1 1 1 -> h + g + f + e + d + c + b
     *     1 1 1 1 1 1 -> h + g + f + e + d + c
     *       1 1 1 1 1 -> h + g + f + e + d
     *         1 1 1 1 -> h + g + f + e
     *           1 1 1 -> h + g + f
     *             1 1 -> h + g
     *               1 -> h
     *
     * @formatter:on
     *
     * Key observation 4: Each number in the output sequence can be computed
     * based on the previous on, reducing the quadratic complexity -> linear.
     */
    String part2(String input) {

        final int offset = Integer.valueOf(input.substring(0, 7));
        final int phases = 100;
        int ilen = input.length();
        int rlen = ilen * 10000; // full length of repeated string
        int len = rlen - offset; // length of string to work with
        int[] in = new int[len];

        // Copy the input string into an integer array, from offset and
        // to the end (of the repeated array).
        for (int i = 0; i < len; i++) {
            in[i] = input.charAt((offset + i) % ilen) - '0';
        }

        for (int n = 0; n < phases; n++) {
            // Each phase can be computed backwards, in which case we can do
            // the computation without any extra storage.
            for (int i = in.length - 2; i >= 0; i--) {
                in[i] = (in[i] + in[i + 1]) % 10;
            }
        }

        char[] msg = new char[8];
        for (int i = 0; i < 8; i++) {
            msg[i] = (char) (in[i] + '0');
        }
        return new String(msg);
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals("84970726", part1(getInputAsString()));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals("47664469", part2(getInputAsString()));
    }
}
