package aoc2020;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import common.AocPuzzle;

/**
 * AoC puzzle template.
 */
public class Day15 extends AocPuzzle {

    public Day15() throws IOException {
        super(2019, 1);
    }

    // TODO solve part 1 here
    private int part1() {
        return 0;
    }

    // TODO solve part 2 here
    private int part2() {
        return 0;
    }

    private static int solve(int[] init, int limit) {
        // map from number to the index in the sequence at
        // which it was last seen
        Map<Integer, Integer> map = new HashMap<>();

        for (int i = 0; i < init.length - 1; i++) {
            // i is 0-based, but turn# is 1-based
            map.put(init[i], i + 1);
        }

        int last = init[init.length - 1];

        for (int turn = init.length + 1; turn <= limit; turn++) {
            int next;

            int indexOfLast = map.getOrDefault(last, -1);
            if (indexOfLast > 0) {
                next = turn - indexOfLast - 1;
            } else {
                next = 0;
            }
            map.put(last, turn - 1);
            last = next;
        }

        return last;
    }

    private static int solve1(int[] init, int limit) {
        int[] array = new int[limit + 1];

        for (int i = 0; i < init.length - 1; i++) {
            // i is 0-based, but turn# is 1-based
            array[init[i]] = i + 1;
        }

        int last = init[init.length - 1];

        for (int turn = init.length + 1; turn <= limit; turn++) {
            int next;

            int indexOfLast = array[last];
            if (indexOfLast != 0) {
                next = turn - indexOfLast - 1;
            } else {
                next = 0;
            }

            array[last] = turn - 1;
            last = next;
        }

        return last;
    }

    /*
     * Tests
     */

    @Test
    public void testPart1And2() throws Exception {
        int[] testdata = { 0, 3, 6 };
        assertEquals(0, solve1(testdata, 10));
        assertEquals(436, solve1(testdata, 2020));
        assertEquals(175594, solve1(testdata, 30000000));
    }

//    @Test
//    public void testPart2() throws Exception {
//        // TODO insert part 2 answer here, once known
//        assertEquals(0, part2());
//    }

}
