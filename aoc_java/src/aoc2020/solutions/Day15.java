package aoc2020.solutions;

import static org.junit.Assert.assertEquals;

import java.io.InputStream;
import java.util.List;
import java.util.Optional;

import org.junit.Test;

import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;

/**
 * AoC puzzle template.
 */
public class Day15 implements IAocPuzzle<List<Integer>, Integer, Integer> {

    @Override
    public List<Integer> parse(Optional<InputStream> reader) {
        return List.of(6, 4, 12, 1, 20, 0, 16);
    }

    @Override
    public Integer part1(List<Integer> input) {
        return solve(input, 2020);
    }

    @Override
    public Integer part2(List<Integer> input) {
        return solve(input, 30000000);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(475, 11261);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 15, "Rambunctious Recitation", false);
    }

    private static int solve(List<Integer> init, int limit) {
        int[] array = new int[limit + 1];

        for (int i = 0; i < init.size() - 1; i++) {
            // i is 0-based, but turn# is 1-based
            array[init.get(i)] = i + 1;
        }

        int last = init.get(init.size() - 1);

        for (int turn = init.size() + 1; turn <= limit; turn++) {
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

    @Test
    public void testPart1And2() throws Exception {
        List<Integer> testdata = List.of(0, 3, 6);
        assertEquals(0, solve(testdata, 10));
        assertEquals(436, solve(testdata, 2020));
        assertEquals(175594, solve(testdata, 30000000));
    }
}
