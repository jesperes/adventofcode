package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

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
public class Day20 implements IAocIntPuzzle<Integer> {

    static final int BLOCK_SIZE = 100000;

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 20, "Infinite Elves and Infinite Houses",
                false);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(831600, 884520);
    }

    @Override
    public Integer parse(Optional<File> file) throws IOException {
        return 36000000;
    }

    @Override
    public Integer part1(Integer input) {
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
                    if (presents[elf - start] >= input) {
                        return elf;
                    }
                }
            }
        }
    }

    @Override
    public Integer part2(Integer input) {
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
                    if (presents[elf - start] >= input) {
                        return elf;
                    }
                }
            }
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day20());
    }
}
