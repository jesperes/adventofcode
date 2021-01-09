package aoc2020;

import java.io.File;
import java.util.List;
import java.util.Optional;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day01 implements IAocPuzzle<List<Long>, Long, Long> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 1, "Report Repair", true);
    }

    @Override
    public List<Long> parse(Optional<File> stream) {
        return InputUtils.asLongList(stream.get());
    }

    @Override
    public Long part1(List<Long> list) {
        for (long x : list) {
            for (long y : list) {
                if (x + y == 2020) {
                    return x * y;
                }
            }
        }
        throw new RuntimeException();
    }

    @Override
    public Long part2(List<Long> list) {
        for (long x : list) {
            for (long y : list) {
                if (x + y >= 2020)
                    continue;

                for (long z : list) {
                    if (x + y + z == 2020) {
                        return x * y * z;
                    }
                }
            }
        }

        throw new RuntimeException();
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(987339L, 259521570L);
    }
}
