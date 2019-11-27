package aoc2016;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import common.Utils;

/**
 * This variant of the 2016 Day 9 problem will compute the length of the
 * decompressed string, but without constructing the individual substrings. The
 * the decompressed string is huge (~12GB), so this optimization is necessary to
 * be able to compute the size without having to copy 12 GB of strings.
 * 
 * @author jespe
 *
 */
public class Day09 {

    final static int MARKER_NUMCHAR = 0;
    final static int MARKER_REPEATCOUNT = 1;
    final static int MARKER_NEXTINDEX = 2;

    int[] getMarker(String s, int pos) {
        int last = -1;

        for (int i = pos; i < s.length(); i++) {
            if (s.charAt(i) == ')') {
                last = i;
                break;
            }
        }

        String[] elems = s.substring(pos, last).split("x");
        int[] result = new int[] { Integer.valueOf(elems[0]),
                Integer.valueOf(elems[1]), last + 1 };
        return result;
    }

    @Test
    public void testGetMarker() throws Exception {
        assertArrayEquals(new int[] { 3, 4, 8 }, getMarker("abc(3x4)def", 4));
    }

    public long computeDecompressedLengthOf(String str) {
        return computeDecompressedLengthOf(str, 0, str.length());
    }

    public long computeDecompressedLengthOf(String str, int start, int length) {
        long len = 0;

        for (int i = start; i < start + length; i++) {
            char c = str.charAt(i);

            if (Character.isWhitespace(c)) {
                continue;
            } else if (c == '(') {
                int marker[] = getMarker(str, i + 1);
                int numChars = marker[MARKER_NUMCHAR];
                int repeatCount = marker[MARKER_REPEATCOUNT];
                int nextIndex = marker[MARKER_NEXTINDEX];

                long decomplen = computeDecompressedLengthOf(str, nextIndex,
                        numChars);
                len += repeatCount * decomplen;
                i = nextIndex + numChars - 1;
            } else {
                len += 1;
            }
        }

        return len;
    }

    @Test
    public void testDecompress() throws Exception {
        assertEquals(9L, computeDecompressedLengthOf("(3x3)ABC"));
        assertEquals("XABCABCABCABCABCABCY".length(),
                computeDecompressedLengthOf("X(8x2)(3x3)ABCY"));
        assertEquals(241920L, computeDecompressedLengthOf(
                "(27x12)(20x12)(13x14)(7x10)(1x12)A"));
        assertEquals(445L, computeDecompressedLengthOf(
                "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"));
    }

    @Test
    public void testPart2() throws Exception {
        String str = Utils.readFile("inputs/2016/day09.txt");
        long x = computeDecompressedLengthOf(str, 0, str.length());
        System.out.println("Decompressed length: " + x);
    }
}
