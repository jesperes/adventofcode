package aoc2016;

import static org.junit.Assert.assertEquals;

import java.security.MessageDigest;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;
import java.util.function.Function;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 14: One-Time Pad
 *
 * Current runtime here is 10-12 seconds (measured from BeforeClass to
 * AfterClass), which probably should be considered pretty good. ~60% of that
 * time is spent in md5.digest().
 */
public class Day14 extends AocPuzzle {

    final private MessageDigest md5;

    private static long md5time = 0;
    private static long md5cntr = 0;
    private static long start = 0;

    public Day14() throws Exception {
        super(2019, 1);
        md5 = MessageDigest.getInstance("MD5");
    }

    @BeforeClass
    public static void beforeClass() {
        start = System.nanoTime();
    }

    @AfterClass
    public static void afterClass() {
        long nanosecs = 1000000000;
        double md5time_seconds = ((double) md5time) / nanosecs;
        long totaltime = System.nanoTime() - start;
        System.out.format(
                "MD5: %d calls, %g seconds total, %d ns/per call, %g%%%n",
                md5cntr, md5time_seconds, md5time / md5cntr,
                (((double) md5time) / totaltime) * 100.0);
    }

    private byte[] md5_digest(byte[] input) {
        md5cntr++;
        long start = System.nanoTime();
        try {
            return md5.digest(input);
        } finally {
            long elapsed = System.nanoTime() - start;
            md5time += elapsed;
        }
    }

    private byte toHexb(byte b) {
        return (byte) ((b <= 9) ? b + 48 : b + 87);
    }

    private void toHexDigest(byte[] digest, byte[] hexdigest) {
        for (int i = 0; i < 16; i++) {
            hexdigest[i * 2] = toHexb((byte) ((digest[i] & 0xf0) >> 4));
            hexdigest[i * 2 + 1] = toHexb((byte) (digest[i] & 0x0f));
        }
    }

    private byte has3(byte[] hexdigest) {
        for (int i = 0; i < hexdigest.length - 2; i++) {
            byte c = hexdigest[i];
            if (hexdigest[i + 1] == c && hexdigest[i + 2] == c)
                return c;
        }
        return -1;
    }

    private byte has5(byte[] hexdigest) {
        for (int i = 0; i < hexdigest.length - 4; i++) {
            byte c = hexdigest[i];
            if (hexdigest[i + 1] == c && hexdigest[i + 2] == c
                    && hexdigest[i + 3] == c && hexdigest[i + 4] == c) {
                return c;
            }
        }
        return -1;
    }

    // Plain MD5 for part 1
    private byte[] MD5(byte[] b) {
        byte[] hexdigest = new byte[32];
        toHexDigest(md5_digest(b), hexdigest);
        return hexdigest;
    }

    // Stretched MD5 for part 2
    private byte[] stretchedMD5(byte[] b) {
        int reps = 2016;
        byte[] hexdigest = new byte[32];
        toHexDigest(md5_digest(b), hexdigest);
        for (int i = 0; i < reps; i++) {
            toHexDigest(md5_digest(hexdigest), hexdigest);
        }
        return hexdigest;
    }

    /**
     * Improved version of day 14 solution. We do not need to compute hashes
     * more than once (hence no memoization). Instead, we scan for 5-sequences
     * and keep track of matching 3-sequences along the way. Typically, once we
     * find a 5-sequence, we will be able to assign many keys based on
     * 3-sequences found less than 1000 indexes away.
     *
     * @param salt The input salt, e.g. "abc" (example input).
     * @param md5  The MD5 function. Differs between part 1 and part 2.
     */
    private int findKey(String salt, Function<byte[], byte[]> md5) {
        // Use TreeSet, because order matters!
        Map<Character, TreeSet<Integer>> threes = new HashMap<>();
        TreeSet<Integer> keys = new TreeSet<>();

        for (int i = 0;; i++) {
            byte[] key = (salt + String.valueOf(i)).getBytes();
            byte[] keyHexDigest = md5.apply(key);

            /*
             * Check for 5-sequences. Loop over all seen hashes with matching
             * 3-sequences.
             */
            byte c5 = has5(keyHexDigest);
            if (c5 != -1) {
                /*
                 * Iterate over matching 3-sequences. Using tree-sets ensures
                 * that we iterate in index order. (There will typically be
                 * several 3-sequences for each found 5-sequence, which are much
                 * more rare.)
                 */
                for (int i3 : threes.get((char) c5)) {
                    int dist = Math.abs(i - i3);
                    if (dist <= 1000) {
                        keys.add(i3);
                        if (keys.size() == 64) {
                            return keys.last();
                        }
                    }
                }
            }

            /*
             * Remember any 3-sequences we find, and store them so we can look
             * them up when we encounter the matching 5-sequence.
             */
            byte c3 = has3(keyHexDigest);
            if (c3 != -1) {
                final int i3 = i;
                threes.compute((char) c3, (k, v) -> {
                    if (v == null)
                        v = new TreeSet<>();
                    v.add(i3);
                    return v;
                });
            }
        }

    }

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        assertEquals(23890, findKey("ahsbgdzn", this::MD5));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(22696, findKey("ahsbgdzn", this::stretchedMD5));
    }
}
