package aoc2020.solutions;

import java.io.BufferedReader;
import java.util.List;
import java.util.Optional;

import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.InputUtils;

public class Day03 implements IAocPuzzle<List<String>, Long, Long> {

    @Override
    public List<String> parse(Optional<BufferedReader> reader) {
        return InputUtils.asStringList(reader.get());
    }

    @Override
    public Long part1(List<String> input) {
        return treesAlongSlope(3, 1, input);
    }

    @Override
    public Long part2(List<String> input) {
        return treesAlongSlope(1, 1, input) * //
                treesAlongSlope(3, 1, input) * //
                treesAlongSlope(5, 1, input) * //
                treesAlongSlope(7, 1, input) * //
                treesAlongSlope(1, 2, input);
    }

    private long treesAlongSlope(int dx, int dy, List<String> input) {
        long trees = 0;
        int width = input.get(0).length();

        for (int y = 0,
                x = 0; y < input.size(); y += dy, x = ((x + dx) % width)) {
            if (input.get(y).charAt(x % width) == '#') {
                trees++;
            }
        }

        return trees;
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(230L, 9533698720L);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 3, "Toboggan Trajectory", true);
    }
}
