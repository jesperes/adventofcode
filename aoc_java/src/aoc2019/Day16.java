package aoc2019;

import static java.lang.Math.abs;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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

    int[] repeat(int[] digits, int n) {
        int[] rdigits = new int[digits.length * n];
        for (int i = 0; i < n; i++) {
            System.arraycopy(digits, 0, rdigits, i * digits.length,
                    digits.length);
        }
        return rdigits;
    }

    /*
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
        int[] digits = new int[input.length()];
        for (int i = 0; i < digits.length; i++) {
            digits[i] = input.charAt(i) - '0';
        }

        int[] rdigits = repeat(digits, 10000);
        int[] in = rdigits;
        int[] out = new int[in.length];
        int[] tmp = null;

        // Just to make sure
        assertTrue(offset > rdigits.length / 2);

        for (int n = 0; n < phases; n++) {
            // Last digit will always be the same?
            out[in.length - 1] = in[in.length - 1];

            // Go backwards from the end to the message offset
            for (int i = in.length - 2; i >= offset; i--) {
                int prev = out[i + 1];
                int inDigit = in[i];
                out[i] = (inDigit + prev) % 10;
            }

            tmp = out;
            out = in;
            in = tmp;
        }

        out = tmp;
        char[] msg = new char[8];
        for (int i = offset; i < offset + 8; i++) {
            msg[i - offset] = (char) (out[i] + '0');
        }
        return new String(msg);
    }

    /*
     * Tests
     */
    @Test
    public void testPattern3() throws Exception {
        assertEquals(0, pattern(0, 3));
        assertEquals(0, pattern(1, 3));
        assertEquals(1, pattern(2, 3));
        assertEquals(1, pattern(3, 3));
        assertEquals(1, pattern(4, 3));
        assertEquals(0, pattern(5, 3));
        assertEquals(0, pattern(6, 3));
        assertEquals(0, pattern(7, 3));
        assertEquals(-1, pattern(8, 3));
        assertEquals(-1, pattern(9, 3));
        assertEquals(-1, pattern(10, 3));
    }

    @Test
    public void testPattern1() throws Exception {
        assertEquals(1, pattern(0, 1));
        assertEquals(0, pattern(1, 1));
        assertEquals(-1, pattern(2, 1));
        assertEquals(0, pattern(3, 1));
        assertEquals(1, pattern(4, 1));
        assertEquals(0, pattern(5, 1));
        assertEquals(-1, pattern(6, 1));
        assertEquals(0, pattern(7, 1));
        assertEquals(1, pattern(8, 1));
        assertEquals(0, pattern(9, 1));
    }

    @Test
    public void testFFT() throws Exception {
        assertEquals("01029498", fft("12345678", 4));
    }

    @Test
    public void testPart1() throws Exception {
        assertEquals("84970726", fft(getInputAsString(), 100).substring(0, 8));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals("84462026", part2("03036732577212944063491565474664"));
        assertEquals("47664469", part2(getInputAsString()));
    }

}
