package aoc2019;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.math.BigInteger;
import java.util.Collections;
import java.util.Map;

import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 2: 1202 Program Alarm
 */
public class Day02 extends AocPuzzle {

    public Day02() throws IOException {
        super(2019, 2);
    }

    private int part1(String input, int x, int y) throws IOException {
        Map<BigInteger, BigInteger> prog = BigIntCode.parse(input);
        prog.put(BigInteger.ONE, BigInteger.valueOf(x));
        prog.put(BigInteger.TWO, BigInteger.valueOf(y));
        BigIntCode intcode = new BigIntCode(prog, Collections.emptyList());
        intcode.execute();
        return prog.get(BigInteger.ZERO).intValue();
    }

    private int part2(String input) throws IOException {
        for (int a = 0; a <= 99; a++)
            for (int b = 0; b <= 99; b++)
                if (part1(input, a, b) == 19690720)
                    return a * 100 + b;

        return 0;
    }

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        assertEquals(3654868, part1(getInputAsString(), 12, 2));
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(7014, part2(getInputAsString()));
    }
}
