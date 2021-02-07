package aoc2019;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import aoc2019.intcode.IntCode;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day09 implements IAocLongPuzzle<String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2019, 9, "Sensor Boost", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(2594708277L, 87721L);
    }

    @Override
    public String parse(Optional<File> file) throws IOException {
        return InputUtils.asString(file.get());
    }

    @Override
    public Long part1(String input) {
        var intcode = new IntCode(IntCode.parse(input), 1);
        intcode.execute();
        return intcode.getOutputs().get(0);
    }

    @Override
    public Long part2(String input) {
        var intcode = new IntCode(IntCode.parse(input), 2);
        intcode.execute();
        return intcode.getOutputs().get(0);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day09());
    }
}
