package aoc2017;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

/**
 * --- Day 6: Memory Reallocation ---
 * 
 * A debugger program here is having an issue: it is trying to repair a memory
 * reallocation routine, but it keeps getting stuck in an infinite loop.
 * 
 * In this area, there are sixteen memory banks; each memory bank can hold any
 * number of blocks. The goal of the reallocation routine is to balance the
 * blocks between the memory banks.
 * 
 * The reallocation routine operates in cycles. In each cycle, it finds the
 * memory bank with the most blocks (ties won by the lowest-numbered memory
 * bank) and redistributes those blocks among the banks. To do this, it removes
 * all of the blocks from the selected bank, then moves to the next (by index)
 * memory bank and inserts one of the blocks. It continues doing this until it
 * runs out of blocks; if it reaches the last memory bank, it wraps around to
 * the first one.
 * 
 * The debugger would like to know how many redistributions can be done before a
 * blocks-in-banks configuration is produced that has been seen before.
 * 
 * For example, imagine a scenario with only four memory banks:
 * 
 * The banks start with 0, 2, 7, and 0 blocks. The third bank has the most
 * blocks, so it is chosen for redistribution.
 * 
 * Starting with the next bank (the fourth bank) and then continuing to the
 * first bank, the second bank, and so on, the 7 blocks are spread out over the
 * memory banks. The fourth, first, and second banks get two blocks each, and
 * the third bank gets one back. The final result looks like this: 2 4 1 2.
 * 
 * Next, the second bank is chosen because it contains the most blocks (four).
 * Because there are four memory banks, each gets one block. The result is: 3 1
 * 2 3.
 * 
 * Now, there is a tie between the first and fourth memory banks, both of which
 * have three blocks. The first bank wins the tie, and its three blocks are
 * distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
 * 
 * The fourth bank is chosen, and its four blocks are distributed such that each
 * of the four banks receives one: 1 3 4 1.
 * 
 * The third bank is chosen, and the same thing happens: 2 4 1 2.
 * 
 * At this point, we've reached a state we've seen before: 2 4 1 2 was already
 * seen. The infinite loop is detected after the fifth block redistribution
 * cycle, and so the answer in this example is 5.
 * 
 * Given the initial block counts in your puzzle input, how many redistribution
 * cycles must be completed before a configuration is produced that has been
 * seen before?
 * 
 * --- Part Two ---
 * 
 * Out of curiosity, the debugger would also like to know the size of the loop:
 * starting from a state that has already been seen, how many block
 * redistribution cycles must be performed before that same state is seen again?
 * 
 * In the example above, 2 4 1 2 is seen again after four cycles, and so the
 * answer in that example would be 4.
 * 
 * How many cycles are in the infinite loop that arises from the configuration
 * in your puzzle input?
 * 
 * @author jesperes
 *
 */
public class Day06 {

    int[] PUZZLE_INPUT = Arrays.stream(
            "2   8   8   5   4   2   3   1   5   5   1   2   15  13  5   14"
                    .split(" +"))
            .mapToInt(Integer::valueOf).toArray();

    private int getStartBank(int[] banks) {
        int max = Integer.MIN_VALUE;
        int startBank = -1;
        for (int i = 0; i < banks.length; i++) {
            if (banks[i] > max) {
                startBank = i;
                max = banks[i];
            }
        }
        return startBank;
    }

    /**
     * Run one redistrubition pass on the memory banks.
     * 
     * @param banks
     */
    private void redistribute(int[] banks) {
        int bankIdx = getStartBank(banks);
        int bankSize = banks[bankIdx];

        // Empty the start bank
        banks[bankIdx] = 0;

        // Distribute the memory blocks one-by-one, starting with
        // the bank next to the start bank.
        while (bankSize > 0) {
            bankIdx = (bankIdx + 1) % banks.length;
            banks[bankIdx]++;
            bankSize--;
        }
    }

    private int redistributeUntilDone(int[] banks) {

        Set<String> seenBanks = new HashSet<>();
        int passes = 0;
        while (true) {
            /*
             * Store arrays as strings, so that we can put them in a set. We
             * can't put the arrays themselves into the set because they do not
             * have proper hashCode/equals semantics (different arrays do not
             * compare equal).
             */
            String str = Arrays.toString(banks);

            if (seenBanks.contains(str)) {
                return passes;
            } else {
                seenBanks.add(str);
                redistribute(banks);
                passes++;
            }
        }
    }

    /**
     * Same as {@link #redistributeUntilDone(int[])}, but returns the size of
     * the loop, how manyu
     * 
     */
    private int redistributeUntilDone_Part2(int[] banks) {
        /*
         * Instead of a set, we keep a map from the array (as a string). As
         * value we put the pass when we saw the configuration. This allows us
         * to compute the size of the loop.
         */

        Map<String, Integer> seenBanks = new HashMap<>();
        int passes = 0;
        while (true) {
            String str = Arrays.toString(banks);

            if (seenBanks.containsKey(str)) {
                int firstSeenPass = seenBanks.get(str);
                return passes - firstSeenPass;
            } else {
                seenBanks.put(str, passes);
                redistribute(banks);
                passes++;
            }
        }
    }

    @Test
    public void testPuzzleInput() throws Exception {
        assertEquals(16, PUZZLE_INPUT.length);
    }

    @Test
    public void testGetStartBank() throws Exception {
        // Ensure that we handle equal-sized banks correctly.
        assertEquals(0, getStartBank(new int[] { 3, 1, 2, 3 }));
        assertEquals(2, getStartBank(new int[] { 0, 2, 7, 0 }));
    }

    @Test
    public void testPart1_OnePass() throws Exception {
        int[] banks = new int[] { 0, 2, 7, 0 };
        redistribute(banks);
        assertArrayEquals(new int[] { 2, 4, 1, 2 }, banks);
    }

    @Test
    public void testPart1_TwoPasses() throws Exception {
        int[] banks = new int[] { 0, 2, 7, 0 };
        redistribute(banks);
        redistribute(banks);
        assertArrayEquals(new int[] { 3, 1, 2, 3 }, banks);
    }

    @Test
    public void testPart1_ThreePasses() throws Exception {
        int[] banks = new int[] { 0, 2, 7, 0 };
        redistribute(banks);
        redistribute(banks);
        redistribute(banks);
        assertArrayEquals(new int[] { 0, 2, 3, 4 }, banks);
    }

    @Test
    public void testPart1_FourPasses() throws Exception {
        int[] banks = new int[] { 0, 2, 7, 0 };
        redistribute(banks);
        redistribute(banks);
        redistribute(banks);
        redistribute(banks);
        assertArrayEquals(new int[] { 1, 3, 4, 1 }, banks);
    }

    @Test
    public void testPart1_FivePasses() throws Exception {
        int[] banks = new int[] { 0, 2, 7, 0 };
        redistribute(banks);
        redistribute(banks);
        redistribute(banks);
        redistribute(banks);
        redistribute(banks);
        assertArrayEquals(new int[] { 2, 4, 1, 2 }, banks);
    }

    @Test
    public void testPart1_UntilDone() throws Exception {
        int[] banks = new int[] { 0, 2, 7, 0 };
        int passes = redistributeUntilDone(banks);
        assertEquals(5, passes);
    }

    @Test
    public void testPart1_full() throws Exception {
        assertEquals(3156, redistributeUntilDone(PUZZLE_INPUT));
    }

    @Test
    public void testPart2_short() throws Exception {
        assertEquals(4, redistributeUntilDone_Part2(new int[] { 0, 2, 7, 0 }));
    }

    @Test
    public void testPart2_full() throws Exception {
        assertEquals(1610, redistributeUntilDone_Part2(PUZZLE_INPUT));
    }
}
