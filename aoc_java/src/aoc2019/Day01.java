package aoc2019;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day01 implements IAocIntPuzzle<List<Integer>> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2019, 1, "The Tyranny of the Rocket Equation",
                true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(3368364, 5049684);
    }

    @Override
    public List<Integer> parse(Optional<File> file) throws IOException {
        return InputUtils.asIntList(file.get());
    }

    @Override
    public Integer part1(List<Integer> input) {
        return input.stream().mapToInt(mass -> mass / 3 - 2).sum();
    }

    private int fuel(int mass) {
        int f = mass / 3 - 2;
        if (f <= 0)
            return 0;
        else
            return f + fuel(f);
    }

    @Override
    public Integer part2(List<Integer> input) {
        return input.stream().mapToInt(mass -> fuel(mass)).sum();
    }
}
