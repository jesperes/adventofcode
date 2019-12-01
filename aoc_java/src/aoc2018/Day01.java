package aoc2018;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 1: Chronal Calibration
 */
public class Day01 extends AocPuzzle {

    public Day01() throws IOException {
        super(2018, 1);
    }

    private long part1() throws IOException {
        return getInputAsStream()
                .collect(Collectors.summarizingInt(s -> Integer.valueOf(s)))
                .getSum();
    }

    private int part2() throws IOException {
        List<Integer> deltas = getInputAsStream()
                .mapToInt(s -> Integer.valueOf(s)).boxed()
                .collect(Collectors.toList());

        Set<Integer> seen = new HashSet<>();
        int freq = 0;

        while (true) {
            for (int n : deltas) {
                freq += n;
                if (seen.contains(freq)) {
                    return freq;
                } else {
                    seen.add(freq);
                }
            }
        }
    }

    private int part1Answer() {
        return 470;
    }

    private int part2Answer() {
        return 790;
    }

    /*
     * -----------------------------------------------------------------------
     * Tests
     * -----------------------------------------------------------------------
     */
    @Test
    public void testPart1() throws Exception {
        assertEquals(part1Answer(), part1());
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(part2Answer(), part2());
    }

}
