package aoc2015;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

/**
 * There are an infinite (enumerable) amount of elves, numbered 1, 2, 3, etc.
 * There are also an infinite amount of houses, also numbered 1, 2, 3, etc. Each
 * elf N delivers presents to house N, N*2, N*3, etc. The puzzle is to find the
 * house with the smallest number which will receive more than X presents (were
 * X is the puzzle input, in my case 36000000).
 * 
 * The problem is quadratic (elves * houses), but it is very sparse, as the
 * distance between houses increases for each elf (so elf 100 delivers only to
 * houses 100, 200, 300, 400, etc.). Even so, the number of houses grows very
 * quickly as the presents become widely distributed, and it takes a lot of
 * elves delivering presents to a lot of houses before any house receives more
 * than 36000000 presents.
 *
 * The critical observation is that once elf N has delivered all its presents
 * house N will not receive any more presents. This is when we can check how
 * many presents house N ultimately gets.
 * 
 * 
 * @author jesperes
 *
 */
public class Day20 {

    static final int INPUT = 36000000;
    static final int BLOCK_SIZE = 100000;

    @Test
    public void testDay20() throws Exception {
        assertEquals(831600, part1());
        assertEquals(884520, part2());
    }

    private int part1() {
        /*
         * Divide the houses into blocks, to limit the number of houses we need
         * to track at a time.
         */
        for (int start = 1;; start += BLOCK_SIZE) {
            int presents[] = new int[BLOCK_SIZE];
            int last = start + BLOCK_SIZE - 1;

            for (int elf = 1; elf <= last; elf++) {
                final int n = elf * 10;
                final int first = start + (elf - (start % elf));

                for (int house = first; house <= last; house += elf) {
                    presents[house - start] += n;
                }

                if (elf >= start) {
                    if (presents[elf - start] >= INPUT) {
                        return elf;
                    }
                }
            }
        }
    }

    private int part2() {
        /*
         * Same as part2, but elfs stop after 50 presents. This could be merged
         * into part1 to gain performance.
         */
        Map<Integer, Integer> map = new HashMap<>();

        for (int start = 1;; start += BLOCK_SIZE) {
            int presents[] = new int[BLOCK_SIZE];
            int last = start + BLOCK_SIZE - 1;

            for (int elf = 1; elf <= last; elf++) {
                final int n = elf * 11;
                final int first = start + (elf - (start % elf));

                for (int house = first; house <= last; house += elf) {
                    int p = map.getOrDefault(elf, 0);
                    if (p < 50) {
                        presents[house - start] += n;
                        map.put(elf, p + 1);
                    }
                }

                if (elf >= start) {
                    if (presents[elf - start] >= INPUT) {
                        return elf;
                    }
                }
            }
        }
    }
}
