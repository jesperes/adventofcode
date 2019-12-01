package aoc2019;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 1: The Tyranny of the Rocket Equation
 */
public class Day01 extends AocPuzzle {

    public Day01() throws IOException {
        super(2019, 1);
    }

    private int part1() throws IOException {
        return getInputAsIntStream().map(mass -> mass / 3 - 2).sum();
    }

    private int part2() throws IOException {
        return getInputAsIntStream().map(mass -> fuel(mass)).sum();
    }

    private int fuel(int mass) {
        int f = mass / 3 - 2;
        if (f <= 0)
            return 0;
        else
            return f + fuel(f);
    }

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        assertEquals(3368364, part1());
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(5049684, part2());
    }

}
