package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day05 implements IAocIntPuzzle<int[]> {

    public int execute(int[] input, boolean part2) {
        // Make a copy of the input, since we need to change it.
        int[] program = new int[input.length];
        System.arraycopy(input, 0, program, 0, input.length);
        int pc = 0;
        int steps = 0;

        while (pc >= 0 && pc < program.length) {
            // Each instruction is a relative jump.
            int offset = program[pc];
            int nextPc = pc + offset;

            // Increment each offset after taking it. For part2, if the
            // offset is 3 or more, instead decrease it by 1.
            if (part2 && offset >= 3)
                program[pc]--;
            else
                program[pc]++;

            // Update pc/steps
            pc = nextPc;
            steps++;
        }

        return steps;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 5,
                "A Maze of Twisty Trampolines, All Alike", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(339351, 24315397);
    }

    @Override
    public int[] parse(Optional<File> file) throws IOException {
        return InputUtils.asIntList(file.get()).stream().mapToInt(n -> n)
                .toArray();
    }

    @Override
    public Integer part1(int[] input) {
        return execute(input, false);
    }

    @Override
    public Integer part2(int[] input) {
        return execute(input, true);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day05());
    }
}
