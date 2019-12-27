package aoc2019;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import common.AocPuzzle;
import common.IntCode;

/**
 * AoC puzzle template.
 */
public class Day09 extends AocPuzzle {

    public Day09() throws IOException {
        super(2019, 9);
    }

    private long execute(long input) throws IOException {
        IntCode intcode = new IntCode(IntCode.parse(getInputAsString()), input);
        intcode.execute();
        return intcode.getOutputs().get(0);
    }

    @Test
    public void tests() throws Exception {
        assertEquals(2594708277L, execute(1));
        assertEquals(87721L, execute(2));
    }
}
