package aoc2019;

import static aoc2019.intcode.IntCode.execute;
import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 5: Sunny with a Chance of Asteroids
 *
 * (Contination of the Day 2 IntCode puzzle)
 */
public class Day05 extends AocPuzzle {
    public Day05() throws IOException {
        super(2019, 5);
    }

    @Test
    public void testPart1() throws IOException {
        String input = getInputAsString();
        assertEquals(16348437L,
                execute(input, 1).stream().mapToLong(n -> n).sum());
        assertEquals(6959377L, execute(input, 5).get(0).longValue());
    }
}
