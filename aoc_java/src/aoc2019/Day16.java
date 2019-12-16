package aoc2019;

import static java.lang.Math.abs;
import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import common.AocPuzzle;

/**
 * AoC puzzle template.
 */
public class Day16 extends AocPuzzle {

    int[] segments = new int[] { 0, 1, 0, -1 };
    int offset = 1; // pattern offset

    public Day16() throws IOException {
        super(2019, 16);
    }

    // TODO solve part 1 here
    private int part1() {
        return 0;
    }

    // TODO solve part 2 here
    private int part2() {
        return 0;
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
            System.out.format("Repeating FFT (%d), length of input = %d...%n",
                    n, s.length());

            // Loop over all the position in the output string.
            for (int j = 0; j < out.length; j++) {
                out[j] = 0;
                if (j % 1000 == 0)
                    System.out.println("Position: " + j);

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
        return segments[((i + offset) / j) % 4];
    }

    String repeatString(String s, int n) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < n; i++) {
            b.append(s);
        }
        return b.toString();
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
//        assertEquals("84462026",
//                part2("03036732577212944063491565474664", 10000));
        assertEquals("84970726", part2(getInputAsString(), 10000));
    }

    String part2(String input, int n) {
        String repeatedInput = repeatString(input, n);
        String f = fft(repeatedInput, 100);
        int offset = Integer.valueOf(input.substring(0, 7));
        return f.substring(offset, offset + 8);
    }

}
