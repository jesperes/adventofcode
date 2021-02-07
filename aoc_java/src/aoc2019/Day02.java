package aoc2019;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.Optional;

import aoc2019.intcode.IntCode;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

/**
 * Day 2: 1202 Program Alarm
 */
public class Day02 implements IAocIntPuzzle<String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2019, 2, "Program Alarm", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(3654868, 7014);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get());
    }

    @Override
    public Integer part1(String input) {
        return runIntCode(input, 12, 2);
    }

    int runIntCode(String input, long x, long y) {
        Map<Long, Long> prog = IntCode.parse(input);
        prog.put(1L, x);
        prog.put(2L, y);
        IntCode intcode = new IntCode(prog);
        intcode.execute();
        return prog.get(0L).intValue();
    }

    @Override
    public Integer part2(String input) {
        for (int a = 0; a <= 99; a++)
            for (int b = 0; b <= 99; b++)
                if (runIntCode(input, a, b) == 19690720)
                    return a * 100 + b;
        throw new RuntimeException();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day02());
    }
}
