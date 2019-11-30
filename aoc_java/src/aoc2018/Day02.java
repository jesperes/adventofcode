package aoc2018;

import static common.IntPair.pair;
import static java.util.stream.Collectors.reducing;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import common.AocPuzzle;
import common.IntPair;

/**
 * Day 2: Inventory Management System
 */
public class Day02 extends AocPuzzle {

    public Day02() throws IOException {
        super(2019, 2);
    }

    private int part1() throws IOException {
        IntPair result = getInputAsStream()
                .map(s -> pair(seq_n(s, 2), seq_n(s, 3))).collect(reducing(
                        pair(0, 0), (a, b) -> pair(a.x + b.x, a.y + b.y)));

        return result.x * result.y;
    }

    // Return 1 if s has a letter which occurs exactly N times, 0 otherwise
    private int seq_n(String s, int n) {
        final Map<Character, Integer> m = new HashMap<>();
        s.chars().forEach(c -> m.merge((char) c, 1, Integer::sum));
        return m.containsValue(n) ? 1 : 0;
    }

    private String part2() throws IOException {
        List<String> lines = getInputAsLines();
        int len = lines.get(0).length();

        for (String a : lines) {
            for (String b : lines) {
                StringBuilder same = new StringBuilder();

                for (int i = 0; i < len; i++) {
                    if (a.charAt(i) == b.charAt(i)) {
                        same.append(a.charAt(i));
                    }
                }

                if (same.length() == len - 1) {
                    return same.toString();
                }
            }
        }

        return null;
    }

    private int part1Answer() {
        return 9139;
    }

    private String part2Answer() {
        return "uqcidadzwtnhsljvxyobmkfyr";
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
