package aoc2019;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Test;

import common.AocPuzzle;

/**
 * Day 2: 1202 Program Alarm
 */
public class Day02 extends AocPuzzle {

    public Day02() throws IOException {
        super(2019, 2);
    }

    private int part1() throws IOException {
        return run(getInput(), 12, 2);
    }

    private int part2() throws IOException {
        List<Integer> input = getInput();
        for (int a = 0; a <= 99; a++)
            for (int b = 0; b <= 99; b++)
                if (run(input, a, b) == 19690720)
                    return a * 100 + b;

        return 0;
    }

    private int run(final List<Integer> input, int a, int b) {
        int[] prog = input.stream().mapToInt(i -> i).toArray();
        prog[1] = a;
        prog[2] = b;

        int pc = 0;
        while (true) {
            int op0 = prog[pc];
            if (op0 == 99)
                return prog[0];

            int op1 = prog[pc + 1];
            int op2 = prog[pc + 2];
            int op3 = prog[pc + 3];

            if (op0 == 1)
                prog[op3] = prog[op1] + prog[op2];
            else if (op0 == 2)
                prog[op3] = prog[op1] * prog[op2];
            else
                fail();

            pc += 4;
        }
    }

    List<Integer> getInput() throws IOException {
        return Arrays.stream(getInputAsString().split(","))
                .mapToInt(Integer::valueOf).boxed()
                .collect(Collectors.toList());
    }

    /*
     * Tests
     */

    @Test
    public void testPart1() throws Exception {
        assertEquals(3654868, part1());
    }

    @Test
    public void testPart2() throws Exception {
        assertEquals(7014, part2());
    }

}
