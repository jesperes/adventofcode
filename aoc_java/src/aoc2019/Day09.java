package aoc2019;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import common.AocPuzzle;

/**
 * AoC puzzle template.
 */
public class Day09 extends AocPuzzle {

    public Day09() throws IOException {
        super(2019, 9);
    }

    class Day09Intcode extends IntCode {
        public Day09Intcode(long value) throws IOException {
            super(IntCode.parse(getInputAsString()), value);
        }
    }

    private long part1() throws IOException {
        IntCode intcode = new Day09Intcode(1L);
        intcode.execute();
        return intcode.getOutputs().get(0);
    }

    private long part2() throws IOException {
        IntCode intcode = new Day09Intcode(2L);
        intcode.execute();
        return intcode.getOutputs().get(0);
    }

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        assertEquals(2594708277L, part1());
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(87721L, part2());
    }

}
