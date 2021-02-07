package aoc2019;

import static java.lang.Math.abs;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

/**
 * Day 16: Flawed Frequency Transmission
 */
public class Day16 implements IAocPuzzle<String, String, String> {

    int[] segments = new int[] { 0, 1, 0, -1 };

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

    int pattern(int i, int j) {
        return segments[((i + 1) / j) % 4];
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2019, 16, "Flawed Frequency Transmission",
                true);
    }

    @Override
    public AocResult<String, String> getExpected() {
        return AocResult.of("84970726", "47664469");
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get());
    }

    @Override
    public String part1(String input) {
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
    @Override
    public String part2(String input) {

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

    public static void main(String[] args) {
        AocBaseRunner.run(new Day16());
    }
}
