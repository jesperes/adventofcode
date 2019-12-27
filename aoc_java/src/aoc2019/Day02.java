package aoc2019;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.Map;

import org.junit.Test;

import common.AocPuzzle;
import common.IntCode;

/**
 * Day 2: 1202 Program Alarm
 */
public class Day02 extends AocPuzzle {

    public Day02() throws IOException {
        super(2019, 2);
    }

    private int part1(String input, long x, long y) throws IOException {
        Map<Long, Long> prog = IntCode.parse(input);
        prog.put(1L, x);
        prog.put(2L, y);
        IntCode intcode = new IntCode(prog);
        intcode.execute();
        return prog.get(0L).intValue();
    }

    private int part2(String input) throws IOException {
        for (int a = 0; a <= 99; a++)
            for (int b = 0; b <= 99; b++)
                if (part1(input, a, b) == 19690720)
                    return a * 100 + b;

        return 0;
    }

    @Test
    public void tests() throws Exception {
        assertEquals(3654868, part1(getInputAsString(), 12, 2));
        assertEquals(7014, part2(getInputAsString()));
    }
}
