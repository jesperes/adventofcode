package aoc2017;

import static org.junit.Assert.assertEquals;

import java.util.stream.Stream;

import org.junit.Test;

import common.AocPuzzle;

public class Day05 extends AocPuzzle {

    public Day05() {
        super(2017, 5);
    }

    public int execute(int[] program) {
        int pc = 0;
        int steps = 0;

        while (pc >= 0 && pc < program.length) {
            // System.out.format("Step %d: %s%n", steps,
            // Arrays.toString(program));

            // Each instruction is a relative jump.
            int nextPc = pc + program[pc];

            // Each jump/offset is incremented by one
            // after taking it.
            program[pc]++;

            // Update pc/steps
            pc = nextPc;
            steps++;
        }

        return steps;
    }

    public int executePart2(int[] program) {
        int pc = 0;
        int steps = 0;

        while (pc >= 0 && pc < program.length) {
            // System.out.format("Step %d: %s%n", steps,
            // Arrays.toString(program));

            // Each instruction is a relative jump.
            int offset = program[pc];
            int nextPc = pc + offset;

            // Increment each offset after taking it. If the
            // offset is 3 or more, instead decrease it by
            // 1.
            if (offset >= 3)
                program[pc]--;
            else
                program[pc]++;

            // Update pc/steps
            pc = nextPc;
            steps++;
        }

        return steps;
    }

    @Test
    public void testPart1_short() {
        assertEquals(5, execute(new int[] { 0, 3, 0, 1, -3 }));
    }

    @Test
    public void testPart2_short() {
        assertEquals(10, executePart2(new int[] { 0, 3, 0, 1, -3 }));
    }

    @Test
    public void testPart1_full() throws Exception {
        try (Stream<String> lines = getInputAsStream()) {
            int[] program = lines.map(s -> Integer.valueOf(s))
                    .mapToInt(n -> n.intValue()).toArray();
            int steps = execute(program);
            assertEquals(339351, steps);
        }
    }

    @Test
    public void testPart2_full() throws Exception {
        try (Stream<String> lines = getInputAsStream()) {
            int[] program = lines.map(s -> Integer.valueOf(s))
                    .mapToInt(n -> n.intValue()).toArray();
            int steps = executePart2(program);
            assertEquals(24315397, steps);
        }
    }
}
